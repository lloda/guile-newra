
; Replacement for Guile C-based array system - WIP
; (c) Daniel Llorens - 2016-2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(import (newra newra) (newra print) (newra tools)
        (only (rnrs base) vector-map) (only (srfi srfi-1) fold)
        (rnrs io ports)
        (srfi srfi-26) (ice-9 match))

(define ra0 (array->ra #(1 2 3)))
(define ra1 (array->ra #@1(1 2 3)))
(define ra2 (array->ra #2((1 2) (3 4))))
(define ra3 (array->ra #2@1@1((1 2) (3 4))))

(make-ra-c #t 0 2 3)
(make-ra-c #t 0 3 2)
(make-ra-c #t 0 '(1 3) 2)
(define ra4 (make-ra-c #t 0 '(1 3) '(1 2)))
(array-index-map! (ra-data ra4) (lambda i i))

; now array-cell, etc.

(define ra0 (make-ra-c (make-dim 10) 10))
(define ra2 (make-ra (make-dim 10) 0 (vector (make-dim 10))))

(define (length=? a b) (= (length a) (length b)))

(read-hash-extend
 #\%
 (lambda (chr port)
   (pk 'READER chr)
   (let loop ((lo '()) (len '()) (rank #f) (i 0))
     (let ((c (lookahead-char port)))
       (if (eof-object? c)
         (case c
           ((#\:)
            (get-char port)
            (unless (or (not rank) (< i rank))
              (throw 'too-many-dimensions-for-rank rank))
            (let ((leni (read port)))
              (unless (number? leni)
                (throw 'bad-lower-bound leni))
              (let ((len (append len (list leni))))
                (cond ((= (length len) (+ 1 (length lo)))
                       (loop (append lo (list 0)) len rank (+ i 1)))
                      ((= (length len) (length lo))
                       (loop lo len rank (+ i 1)))
                      (else
                       (throw 'mismatched-len-lo))))))
           ((#\@)
            (get-char port)
            (unless (= (length len) (length lo))
              (throw 'lo-on-unbalanced-prefix))
            (unless (or (not rank) (< i rank))
              (throw 'too-many-dimensions-for-rank rank))
            (let ((loi (read port)))
              (unless (number? loi)
                (throw 'bad-lower-bound loi))
              (loop (append lo (list loi)) len rank i)))
           ((#\()
            (unless (= i (length len) (length lo))
              (throw 'lo-on-unbalanced-prefix))
            (when (pair? len)
              (when rank
                (unless (= i rank)
                  (throw 'too-few-dimensions-for-rank i rank))))
; READ CONTENT HERE
            (get-char port)
            (list lo len))
           (else
            (when (or rank (positive? i))
              (throw 'unexpected-rank rank))
            (let ((rank (read port)))
              (loop lo len rank i)))))))))
