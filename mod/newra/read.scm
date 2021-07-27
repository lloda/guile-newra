; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2017-2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Reader for ra objects. They start with #% instead of #, otherwise the syntax
;; is the same as for regular Guile arrays.
;;; Code:

(define-module (newra read)
  #:export (list->ra list->typed-ra))

(import (newra base) (newra map) (newra tools)
        (rnrs io ports) (srfi :8) (srfi :26) (ice-9 match) (ice-9 rdelim)
        (only (rnrs base) vector-map)
        (only (srfi :1) fold unzip2 car+cdr))

(define vector-fold (@ (newra vector) vector-fold))

; take a looked ahead 'c'. FIXME shouldn't look ahead the last one and then again in the caller.
(define (read-number port)
  (let ((c (lookahead-char port)))
    (receive (m c)
        (if (eqv? c #\-)
          (begin (get-char port) (values -1 (lookahead-char port)))
          (values 1 c))
      (unless (char-numeric? c)
        (throw 'failed-to-read-number))
      (* m (let loop ((n 0) (c c))
             (if (char-numeric? c)
               (loop (+ (* 10 n) (string->number (string (get-char port)))) (lookahead-char port))
               n))))))

(define (skip-whitespace port)
  (let loop ((c (lookahead-char port)))
    (cond ((char-whitespace? c) (get-char port) (loop (lookahead-char port)))
          (else c))))

(define pick-root-functions (@ (newra base) pick-root-functions))
(define pick-make-root (@ (newra base) pick-make-root))

(define (make-root type size)
  ((pick-make-root type) size))

(define (root-type root)
  (receive (type vlen vref vset!) (pick-root-functions root) type))

(define (root-length root)
  (receive (type vlen vref vset!) (pick-root-functions root) (vlen root)))

(define (root-ref root i)
  (receive (type vlen vref vset!) (pick-root-functions root) (vref root i)))

(define (root-set! root o i)
  (receive (type vlen vref vset!) (pick-root-functions root) (vset! root i o)))

; Don't resize but make a list of vectors and cat once at the end.
(define (root-resize old newsize)
  (let ((oldsize (root-length old)))
    (if (= newsize oldsize)
      old
      (let ((new (make-root (root-type old) newsize))
            (size (min oldsize newsize)))
        (let loop ((j 0))
          (cond ((= j size) new)
                (else (root-set! new (root-ref old j) j)
                      (loop (+ j 1)))))))))

(define (make-temp-root len type)
  (let ((rank (vector-length len)))
    (receive (temp final-size?)
        (let loop ((size 1) (k 0))
          (if (= k rank)
            (values (make-root type size) #t)
            (let ((l (vector-ref len k)))
              (if l (loop (* size l) (+ 1 k))
                  (values (make-root type 8) #f)))))
      (values temp
              (if final-size?
                (lambda (temp j) temp)
                (lambda (temp j)
                  (let ((n (root-length temp)))
                    (if (> j n)
                      (root-resize temp (ceiling (* (+ n j) 3/2)))
                      temp))))))))

(define (delim-pair c)
  (match c
    (#\[ #\])
    (#\( #\))
    (#\] #\[)
    (#\) #\()))

(define (delim-open? c)
  (match c
    ((or #\[ #\() #t)
    (else #f)))

(define (delim-close? c)
  (match c
    ((or #\] #\)) #t)
    (else #f)))

(read-hash-extend
    #\%
  (lambda (chr port)
    (let* ((c (lookahead-char port))
           (rank (if (char-numeric? c)
                   (let ((rank (read-number port)))
                     (if (negative? rank)
                       (throw 'bad-rank rank)
                       rank))
                   1))
           (type (read-delimited ":@([" port 'peek))
           (type (if (zero? (string-length type)) #t (string->symbol type)))
           (lo (make-vector rank 0))
           (len (make-vector rank #f)))
      (let loop ((k 0))
        (let ((c (lookahead-char port)))
          (cond
           ((eqv? c #\@)
            (unless (< k rank) (throw 'too-many-dimensions-for-rank k rank))
            (get-char port)
            (vector-set! lo k (read-number port))
            (let ((c (lookahead-char port)))
              (cond ((eqv? c #\:)
                     (unless (< k rank) (throw 'too-many-dimensions-for-rank k rank))
                     (get-char port)
                     (vector-set! len k (read-number port)))
                    (else
                     (vector-set! len k #f)))
              (loop (+ k 1))))
           ((eqv? c #\:)
            (unless (< k rank) (throw 'too-many-dimensions-for-rank k rank))
            (get-char port)
            (vector-set! len k (read-number port))
            (vector-set! lo k 0)
            (loop (+ k 1)))
           (else
            (unless (or (zero? k) (= k rank)) (throw 'too-few-dimensions-for-rank k rank))
            (unless (delim-open? c) (throw 'expected-open-paren c))
            (let ((delim-stack (list c)))
; read content here
              (cond
               ((zero? rank)
                (get-char port)
                (let ((item (read port)))
                  (get-char port)
                  (make-ra-new #t item #())))
               (else
                (receive (temp resize-temp) (make-temp-root len type)
                  (let ((j 0))
                    (let loop-rank ((k rank))
                      (cond
; read element
                       ((zero? k)
                        (set! temp (resize-temp temp (+ j 1)))
                        (root-set! temp (read port) j)
                        (set! j (+ j 1)))
; read slice
                       (else
                        (let ((c (skip-whitespace port)))
                          (unless (delim-open? c) (throw 'expected-open-paren-at-dim (- rank k) c))
                          (set! delim-stack (cons c delim-stack))
                          (get-char port))
                        (let ((lenk (vector-ref len (- rank k))))
                          (cond
; read a whole slice when the dimension is known
                           ((and (= k 1) lenk)
                            (set! temp (resize-temp temp (+ j lenk)))
                            (do ((i 0 (+ i 1))) ((= i lenk))
                              (root-set! temp (read port) (+ j i)))
                            (set! j (+ j lenk))
                            (let ((c (skip-whitespace port)))
                              (unless (delim-close? c)
                                (throw 'too-many-elements-in-dim (- rank k) c lenk))
                              (unless (eqv? (delim-pair c) (car delim-stack))
                                (throw 'mismatched-delimiters-in-dim (- rank k) c lenk))
                              (set! delim-stack (cdr delim-stack))
                              (get-char port)))
; general case, feeling for the end
                           (else
                            (let loop-dim ((i 0))
                              (let ((c (skip-whitespace port)))
                                (cond
                                 ((delim-close? c)
                                  (unless (eqv? (delim-pair c) (car delim-stack))
                                    (throw 'mismatched-delimiters-in-dim (- rank k) c lenk))
                                  (set! delim-stack (cdr delim-stack))
                                  (get-char port)
                                  (cond
                                   ((not lenk)
                                    (vector-set! len (- rank k) i))
                                   ((< i lenk)
                                    (throw 'too-few-elements-in-dim (- rank k) i lenk))))
                                 ((or (not lenk) (< i lenk))
                                  (loop-rank (- k 1))
                                  (loop-dim (+ i 1)))
                                 (else
                                  (throw 'too-many-elements-on-dim (- rank k))))))))))))
                    (make-ra-root
                     (root-resize temp (vector-fold (lambda (a b) (* (if a a 0) b)) 1 len))
                     (apply c-dims
                       (vector->list (vector-map (lambda (lo len) (if len (list lo (+ lo len -1)) (list 0 -1)))
                                                 lo len))))))))))))))))

; The docstring is from Guile's list->typed-array.

(define list->ra
  (case-lambda
   "
list->ra shape l -> ra
list->ra type shape l -> ra

Convert the nested list L to a RA of type TYPE. TYPE defaults to #t.

The argument SHAPE determines the number of dimensions of the array and their
shape.  It is either an exact integer, giving the number of dimensions directly,
or a list whose length specifies the number of dimensions and each element
specified the lower and optionally the upper bound of the corresponding
dimension.  When the element is list of two elements, these elements give the
lower and upper bounds.  When it is an exact integer, it gives only the lower
bound.

See also: list->typed-ra ra->list ra-copy ra-copy! as-ra
"
   ((shape l) (list->typed-ra #t shape l))
   ((type shape l) (list->typed-ra type shape l))))

; FIXME looks up all lengths first. Is that necessary?

(define (list->typed-ra type shape l)
   "
list->typed-ra type shape l -> ra

Equivalent to (list->ra TYPE SHAPE L).

See also: list->ra ra->list ra-copy ra-copy! as-ra
"
  (define (list-len l rank)
    (let loop ((k rank) (l l))
      (if (zero? k) '() (cons (length l) (loop (- k 1) (car l))))))
  (receive (rank lo len)
      (cond
       ((number? shape)
        (values shape (make-list shape 0) (list-len l shape)))
       ((list shape)
        (let* ((rank (length shape))
               (len (list-len l rank))
               (lo (map (lambda (x) (if (number? x) x (car x))) shape)))
          (for-each
           (lambda (s lo len)
             (unless (number? s)
               (unless (= len (- (cadr s) lo -1)) (throw 'mismatched-shape shape))))
           shape lo len)
          (values rank lo len)))
       (else (throw 'bad-shape-spec shape)))
    (let ((temp (make-root type (fold * 1 len)))
          (j 0))
      (let loop-rank ((len len) (l l))
        (cond
; read element
         ((null? len)
          (root-set! temp l j)
          (set! j (+ j 1)))
         (else
          (receive (lenk len) (car+cdr len)
            (cond
; read 1-slice
             ((null? len)
              (do ((i 0 (+ i 1)) (l l (cdr l)))
                  ((= i lenk)
                   (unless (null? l) (throw 'mismatched-list-length-dim l lenk (- rank 1))))
                (root-set! temp (car l) (+ j i)))
              (set! j (+ j lenk)))
; general case
             (else
              (do ((i 0 (+ i 1)) (l l (cdr l)))
                  ((= i lenk)
                   (unless (null? l) (throw 'mismatched-list-length-dim (- rank 1 (length len)))))
                (loop-rank len (car l)))))))))
; FIXME c-dims takes len | (lo hi) as in Guile, but I'd prefer len | (len lo)
      (make-ra-root
       temp (apply c-dims (map (lambda (lo len) (list lo (+ lo len -1))) lo len)))))) ; FIXME
