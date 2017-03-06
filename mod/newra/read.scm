
; (c) Daniel Llorens - 2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Reader for ra objects. They start with #% instead of #, otherwise the syntax
;; is the same as for regular Guile arrays.
;; FIXME: doesn't read the type.
;;; Code:

(define-module (newra read))

(import (newra newra) (newra tools)
        (only (rnrs base) vector-map) (only (srfi srfi-1) fold every)
        (rnrs io ports) (srfi srfi-8)
        (srfi srfi-26) (ice-9 match) (ice-9 rdelim))

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

; FIXME eventually replace.

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
   (let loop ((lo '()) (len '()) (rank #f) (i 0) (type #f))
     (let ((c (lookahead-char port)))
       (if (eof-object? c)
         (throw 'unexpected-end-of-input)
         (cond
          ((eqv? c #\:)
           (get-char port)
           (unless (or (not rank) (< i rank))
             (throw 'too-many-dimensions-for-rank i rank))
           (let ((leni (read-number port)))
             (unless (not (negative? leni))
               (throw 'bad-length leni))
             (let ((len (append len (list leni))))
               (cond ((= (length len) (+ 1 (length lo)))
                      (loop (append lo (list 0)) len rank (+ i 1) type))
                     ((= (length len) (length lo))
                      (loop lo len rank (+ i 1) type))
                     (else
                      (throw 'mismatched-len-lo))))))
          ((eqv? c #\@)
           (get-char port)
           (unless (or (not rank) (< i rank))
             (throw 'too-many-dimensions-for-rank rank))
           (let ((lo (append lo (list (read-number port)))))
             (if (= (+ 1 (length len)) (length lo))
               (loop lo len rank i type)
               (loop lo (if (= (length len) (length lo)) len (append len (list #f))) rank (+ i 1) type))))
          ((eqv? c #\()
           (receive (i len)
               (if (= (length len) (length lo)) (values i len) (values (+ i 1) (append len (list #f))))
             (let ((rank (or rank 1))
                   (type (or type #t)))
               (when (and (pair? len) rank (not (= i rank)))
                 (throw 'too-few-dimensions-for-rank i rank))
; read content here
               (cond
                ((zero? rank)
                 (get-char port)
                 (let ((item (read port)))
                   (get-char port)
                   (make-ra-new #t item)))
                (else
                 (let* ((lo (if (null? lo) (make-list rank 0) lo))
                        (len (if (null? len) (make-list rank #f) len))
                        (temp (make-root type (if (every identity len) (fold * 1 len) 8)))
                        (j 0))
                   (let loop-rank ((k rank))
                     (cond
; read element
                      ((zero? k)
                       (let ((n (root-length temp)))
                         (when (>= j n)
                           (set! temp (root-resize temp (ceiling (* n 3/2))))))
                       (root-set! temp j (read port))
                       (set! j (+ j 1)))
; read slice
                      (else
                       (let ((c (skip-whitespace port)))
                         (unless (eqv? #\( c)
                           (throw 'expected-open-paren c i j k))
                         (get-char port))
                       (let loop-dim ((i 0))
                         (let ((c (skip-whitespace port))
                               (dimk (list-ref len (- rank k))))
                           (cond
                            ((eqv? #\) c)
                             (get-char port)
; dimension may be too short
                             (cond
                              ((not dimk)
                               (list-set! len (- rank k) i))
                              ((< i dimk)
                               (throw 'too-few-elements-in-dimension (- rank k) i dimk))))
; dimension may be too long
                            ((or (not dimk) (< i dimk))
                             (loop-rank (- k 1))
                             (loop-dim (+ i 1)))
                            (else
                             (throw 'too-many-elements-on-dim (- rank k)))))))))
                   (apply make-ra-data (root-resize temp (fold * 1 len))
                          (map (lambda (lo len) (list lo (+ lo len -1))) lo len))))))))
; try to read rank if we don't have one already
          ((char-numeric? c)
           (when (or rank (positive? i))
             (throw 'unexpected-rank rank))
           (let ((rank (read-number port)))
             (loop lo len rank i type)))
; try to read type if we don't have one already
          (type
           (get-char port)
           (throw 'bad-character c type))
          (else
           (let ((type (string->symbol (read-delimited ":@(" port 'peek))))
             (loop lo len rank i type)))))))))
