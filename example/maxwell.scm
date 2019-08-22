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

(define (roll A dim n)
  ...)


(define (maxwell)
  (let ((delta 1)
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

    (ra-map! (ra-from A 0 #t #t #t 2)
             (lambda (x) (* (cos x) -1 (/ dn)))
             (ra-iota n 0 dn))
    (ra-map! (ra-from A 1 #t #t #t 2)
             (lambda (x) (* (cos x) -1 (/ dn)))
             (ra-iota n (- delta) (* dn (- delta))))

    ...
))
