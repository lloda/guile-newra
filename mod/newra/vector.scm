; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2016-2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Vector utilities used for the dim vector - FIXME remove if unused.
;;; Code:

(define-module (newra vector)
  #:use-module (srfi srfi-26)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((rnrs base) #:select (vector-for-each))
  #:use-module ((srfi srfi-43) #:select (vector-copy! vector-copy))
  #:export (vector-drop vector-fold* vector-fold vector-append vector-take)
  #:re-export (vector-for-each)
  #:re-export-and-replace (vector-copy))

(define vector-fold*
  (case-lambda
   ((n org kons knil)
    (throw 'missing-arguments))
   ((n org kons knil v)
    (let ((end (+ n org)))
      (let loop ((i org) (k knil))
        (if (= i end)
          k
          (loop (+ i 1) (kons (vector-ref v i) k))))))
   ((n org kons knil . vs)
    (let ((end (+ n org)))
      (let loop ((i org) (k knil))
        (if (= i end)
          k
          (loop (+ i 1) (apply kons (append (map (cut vector-ref <> i) vs) (list k))))))))))

(define vector-fold
  (case-lambda
   ((kons knil)
    (throw 'missing-arguments))
   ((kons knil v)
    (vector-fold* (vector-length v) 0 kons knil v))
   ((kons knil . vs)
    (apply vector-fold* (vector-length (car vs)) 0 kons knil vs))))

; avoid sharing dim vectors, even when copying the full vector. For example, ra-slice depends on this.

(define (vector-drop v n)
  (vector-copy v n (vector-length v)))

(define (vector-take v n)
  (vector-copy v 0 n))

(define (vector-append . a)
  (let ((b (make-vector (fold (lambda (a c) (+ (vector-length a) c)) 0 a))))
    (let loopa ((a a) (lo 0))
      (if (null? a)
        b
        (begin
          (vector-copy! b lo (car a))
          (loopa (cdr a) (+ lo (vector-length (car a)))))))))
