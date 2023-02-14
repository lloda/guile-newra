; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; 4-vector potential vacuum field equations
;; Version of 'Maxwell' from Chaitin 1986, Physics in APL2, p.20, to demo guile-newra.
;; FIXME Redo with lazy ops.
;;; Code:

(import (newra) (srfi 64) (ice-9 format)
        (only (srfi 43) vector-swap!)
        (only (rnrs base) exact))

; transpose/untranspose isn't convenient here :-\
(define (⌽ n k A)
  (let ((A (ra-copy A)))
    (vector-swap! (ra-dims A) 0 k)
    (ra-rotate! n A)
    (vector-swap! (ra-dims A) 0 k)
    A))

(define ι (ra-iota)) ; index for the first dimension
(define ⍉ ra-transpose)
(define .. (dots))
(define : (dots 1))

(define (draw t F)
  (show t "Ey" (F t : 0 0 2 0))
  (show t "Bz" (F t : 0 0 2 1))
  (usleep #e50e3))

(define (show t name F)
  (format #t "~a(0)=~12,10f t=~a:\n" name (F 0) t)
  (ra-for-each
   (lambda (F) (format #t "~a*\n" (make-string (exact (round (* 20 (+ 1 (max (min F +1) -1))))) #\space)))
   F))

(define (maxwell)
  (let* ((δ 1)
         (o 20)
         (n 20)
         (m 2)
         (l 2)
         (A (make-ra 0 o n m l 4))
         (DA (make-ra 0 o n m l 4 4))
         (F (make-ra 0 o n m l 4 4))
         (divA (make-ra 0 o n m l))
         (X (make-ra 0 n m l 4))
         (Y (make-ra 0 n m l 4))
         (dn (* 2 (acos -1) (/ n))))

    (ra-map! (A 0 .. 2) (lambda (x) (* -1 (/ dn) (cos (* x dn)))) ι)
    (ra-map! (A 1 .. 2) (lambda (x) (* -1 (/ dn) (cos (* (- x δ) dn)))) ι)

    (do ((t 1 (+ t 1))) ((= o (+ t 1)))
      (ra-map! X + (⌽ +1 0 (A t)) (⌽ +1 1 (A t)) (⌽ +1 2 (A t)))
      (ra-map! Y + (⌽ -1 0 (A t)) (⌽ -1 1 (A t)) (⌽ -1 2 (A t)))
      (ra-map! (A (+ t 1)) (lambda (x y am1 a) (+ x y (- am1) (* -4 a))) X Y (A (- t 1)) (A t)))

    (ra-map! (DA .. 0) (lambda (a b) (/ (- a b) +2 δ)) (⌽ 1 0 A) (⌽ -1 0 A))
    (ra-map! (DA .. 1) (lambda (a b) (/ (- a b) -2 δ)) (⌽ 1 1 A) (⌽ -1 1 A))
    (ra-map! (DA .. 2) (lambda (a b) (/ (- a b) -2 δ)) (⌽ 1 2 A) (⌽ -1 2 A))
    (ra-map! (DA .. 3) (lambda (a b) (/ (- a b) -2 δ)) (⌽ 1 3 A) (⌽ -1 3 A))

    (format #t "'LORENTZ CONDITION: MAX|DIV| = 0? ~a'\n"
            (ra-fold (lambda (a b) (max a (magnitude b)))
                     0 (ra-map! divA + divA (⍉ DA 0 1 2 3 4 4))))

    (ra-map! F - (⍉ DA 0 1 2 3 5 4) DA)

    (do ((t 0 (+ t 1))) ((= o t))
      (draw t F))
    F))

(test-begin "maxwell")
(test-approximate 0.3039588939177449 (pk 'result ((maxwell) 19 0 0 0 2 1)) 1e-14)
(test-end "maxwell")
