
; (c) Daniel Llorens - 2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Reader for ra objects. They start with #% instead of #, otherwise the syntax
;; is the same as for regular Guile arrays.
;;; Code:

(define-module (newra read))

(import (newra newra) (newra tools)
        (only (rnrs base) vector-map) (only (srfi srfi-1) fold)
        (rnrs io ports) (srfi srfi-8)
        (srfi srfi-26) (ice-9 match) (ice-9 rdelim))

(define vector-fold (@@ (newra newra) vector-fold))

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

; FIXME eventually replace these.

(define (make-root type size) (make-typed-array type *unspecified* size))
(define (root-type root) (array-type root))
(define (root-length root) (array-length root))
(define (root-ref root i) (array-ref root i))
(define (root-set! root i o) (array-set! root o i))
(define (root-resize old newsize)
  (let ((oldsize (root-length old)))
    (if (= newsize oldsize)
      old
      (let ((new (make-root (root-type old) newsize))
            (size (min oldsize newsize)))
        (let loop ((j 0))
          (cond ((= j size) new)
                (else (root-set! new j (root-ref old j))
                      (loop (+ j 1)))))))))

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
          (type (read-delimited ":@(" port 'peek))
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
           (unless (eqv? c #\() (throw 'expected-open-paren c))
           (unless (or (zero? k) (= k rank)) (throw 'too-few-dimensions-for-rank k rank))))))
; read content here
     (cond
      ((zero? rank)
       (get-char port)
       (let ((item (read port)))
         (get-char port)
         (make-ra-new #t item)))
      (else
       (receive (temp final-size?)
           (let loop ((size 1) (k 0))
             (if (= k rank)
               (values (make-root type size) #t)
               (let ((l (vector-ref len k)))
                 (if l (loop (* size l) (+ 1 k))
                     (values (make-root type 8) #f)))))
         (let ((j 0)
               (resize-temp!
                (if final-size?
                  (lambda (j) (when #f #f))
                  (lambda (j)
                    (let ((n (root-length temp)))
                      (when (> j n)
                        (set! temp (root-resize temp (ceiling (* (+ n j) 3/2))))))))))
           (let loop-rank ((k rank))
             (cond
; read element
              ((zero? k)
               (resize-temp! (+ j 1))
               (root-set! temp j (read port))
               (set! j (+ j 1)))
; read slice
              (else
               (let ((c (skip-whitespace port)))
                 (unless (eqv? #\( c) (throw 'expected-open-paren-at-dim (- rank k) c))
                 (get-char port))
               (let ((lenk (vector-ref len (- rank k))))
                 (cond
; read a whole slice when the dimension is known
                  ((and (= k 1) lenk)
                   (resize-temp! (+ j lenk))
                   (do ((i 0 (+ i 1))) ((= i lenk))
                     (root-set! temp (+ j i) (read port)))
                   (set! j (+ j lenk))
                   (let ((c (skip-whitespace port)))
                     (unless (eqv? #\) c) (throw 'too-many-elements-in-dim (- rank k) c lenk))
                     (get-char port)))
; general case, feeling for the end
                  (else
                   (let loop-dim ((i 0))
                     (let ((c (skip-whitespace port)))
                       (cond
                        ((eqv? #\) c)
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
           (apply make-ra-data (root-resize temp (vector-fold * 1 len))
                  (vector->list (vector-map (lambda (lo len) (list lo (+ lo len -1))) lo len))))))))))
