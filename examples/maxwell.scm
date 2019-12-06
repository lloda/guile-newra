; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; 4-vector potential vacuum field equations
;; A version of 'Maxwell' from Chaitin 1986, Physics in APL2, p.20, as a demonstration of guile-newra.
;; FIXME Still missing some pieces.
;;; Code:

(define π (acos -1))
(define : #t)
(define ι ra-iota)
(define ⍉ ra-transpose)
(define ⌽ ra-rotate)

(define (roll A dim n)
  ...)

(define (maxwell)
  (let ((δ 1)
        (o 20)
        (n 20)
        (m 1)
        (l 1)
        (A (make-ra 0 o n m l 4))
        (DA (make-ra 0 o n m l 4 4))
        (F (make-ra 0 o n m l 4 4))
        (divA (make-ra 0 o n m l))
        (X (make-ra 0 n m l 4))
        (Y (make-ra 0 n m l 4))
        (dn (* 2 π (/ n))))

    ;; A[0;;;;2]←-(÷○2÷N)×2○○2×(⍳N)÷N
    (ra-map! (ra-from A : : : 2)
             (lambda (x) (* (cos x) -1 (/ dn)))
             (ra-iota n 0 dn))

    ;; A[1;;;;2]←-(÷○2÷N)×2○○2×((-DELTA)+⍳N)÷N
    (ra-map! (ra-from A 1 : : : 2)
             (lambda (x) (* (cos x) -1 (/ dn)))
             (ra-iota n (- δ) (* dn (- δ))))

    (do ((t 1 (+ t 1))) ((= o (+ t 1)))

      ;; X←(1⌽[0]A[T;;;;])+(1⌽[1]A[T;;;;])+(1⌽[2]A[T;;;;])
      FIXME

      ;; Y←(¯1⌽[0]A[T;;;;])+(¯1⌽[1]A[T;;;;])+(¯1⌽[2]A[T;;;;])
      FIXME

      ;; A[T+1;;;;]←X+Y-A[T-1;;;;]+4×A[T;;;;]
      (ra-map! (ra-from A (+ t 1))
               (lambda (x y am1 a) (+ x y (- am1) (* 4 a)))
               x y (ra-from A (- t 1)) (ra-from A t))
      )

    ;; DA[;;;;;0]←((1⌽[0]A)-(¯1⌽[0]A))÷2×DELTA
    ;; DA[;;;;;1]←-((1⌽[1]A)-(¯1⌽[1]A))÷2×DELTA
    ;; DA[;;;;;2]←-((1⌽[2]A)-(¯1⌽[2]A))÷2×DELTA
    ;; DA[;;;;;3]←-((1⌽[3]A)-(¯1⌽[3]A))÷2×DELTA

    (ra-map! ...)

    ...
))
