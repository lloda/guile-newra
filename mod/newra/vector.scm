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
  #:export (vector-drop vector-fold* vector-fold vector-clip vector-append))

(import (srfi :26)  (only (srfi :1) fold) (only (rnrs base) vector-for-each)
        (only (srfi :43) vector-copy!))

(re-export vector-for-each)

(define vector-fold*
  (case-lambda
   ((n kons knil)
    (throw 'missing-arguments))
   ((n kons knil v)
    (let ((end n))
      (let loop ((i 0) (k knil))
        (if (= i end)
          k
          (loop (+ i 1) (kons (vector-ref v i) k))))))
   ((n kons knil . vs)
    (let ((end n))
      (let loop ((i 0) (k knil))
        (if (= i end)
          k
          (loop (+ i 1) (apply kons (append (map (cut vector-ref <> i) vs) (list k))))))))))

(define vector-fold
  (case-lambda
   ((kons knil)
    (throw 'missing-arguments))
   ((kons knil v)
    (vector-fold* (vector-length v) kons knil v))
   ((kons knil . vs)
    (apply vector-fold* (vector-length (car vs)) kons knil vs))))

; avoid sharing, even when the result would be copy of the full vector
; (e.g. ra-slice depends on this).

(define (vector-clip v lo end)
  (let ((w (make-vector (- end lo) *unspecified*)))
    (vector-copy! w 0 v lo end)
    w))

(define (vector-drop v n)
  (vector-clip v n (vector-length v)))

(define (vector-take v n)
  (vector-clip v 0 n))

(define (vector-append . a)
  (let ((b (make-vector (fold (lambda (a c) (+ (vector-length a) c)) 0 a))))
    (let loopa ((a a) (lo 0))
      (if (null? a)
        b
        (begin
          (vector-copy! b lo (car a))
          (loopa (cdr a) (+ lo (vector-length (car a)))))))))
