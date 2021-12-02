; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2016-2017
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Trying things.

(import (newra) (newra tools) (newra base) (ice-9 match) (ice-9 format)
        (srfi 8) (srfi 26) (srfi 1) (only (srfi 43) vector-copy! vector-fill!)
        (rnrs io ports) (only (rnrs base) vector-map)
        (only (rnrs bytevectors) bytevector-copy! bytevector-fill!))

(define ⍉ ra-transpose)


; -----------------------
; bench vs ffi blis shows one or all of these
; * need for (better) loop order heuristic in %slice-loop etc
; * need for strided rank-1 or rank-2 basic ops
; -----------------------

(import (newra) (ffi blis) (ice-9 match))

(define (bench-copym m n)
  (define A (make-typed-array 'f64 1 m n))
  (define At (transpose-array A 1 0))
  (define B (make-typed-array 'f64 1 m n))
  (define Bt (transpose-array B 1 0))
  (define Ct (make-typed-array 'f64 1 n m))
  (define C (transpose-array Ct 1 0))
  (define discard (const #f))
  (define bench
    `((,(time (array-copy! A B))
       ,(time (discard (ra-copy! (array->ra B) (array->ra A))))
       ,(time (discard (blis-dcopym! 0 BLIS_NONUNIT_DIAG BLIS_DENSE BLIS_NO_TRANSPOSE A B))))
      (,(time (array-copy! At Bt))
       ,(time (discard (ra-copy! (array->ra Bt) (array->ra At))))
       ,(time (discard (blis-dcopym! 0 BLIS_NONUNIT_DIAG BLIS_DENSE BLIS_NO_TRANSPOSE At Bt))))
      (,(time (array-copy! A C))
       ,(time (discard (ra-copy! (array->ra C) (array->ra A))))
       ,(time (discard (blis-dcopym! 0 BLIS_NONUNIT_DIAG BLIS_DENSE BLIS_NO_TRANSPOSE A C))))))

  (let* ((b (list->ra 2 bench))
         (ref (ra-fold max -inf.0 b))
         (b (ra-map #f / (make-ra ref) b)))
    (format #t "ref/t m ~a n ~a for ref ≈ ~4,3f s.\n" m n ref)
    (ra-format (ra-scat #f 1 (list->ra 1 '(guile newra blis)) b)
               #:compact? #t
               #:fmt (lambda (x) (if (real? x) (format #f "~5,1f" x) (format #f "~a" x))))))

(for-each (match-lambda ((m n) (bench-copym m n)))
  '((10 5)
    (100 50)
    (1000 500)
    (10000 5000)
    (100000 50)
    (10 500000)))


; -----------------------
; goops (?)
; -----------------------

(import (oop) (goops))

(define <ra> (class-of (make-ra 0 0)))
(define-method (+ (x <ra>) (y <ra>)) (ra-map #f + x y))
(define-method (+ (x <number>) (y <ra>)) (ra-map #f + (make-ra x) y))
(define-method (+ (x <ra>) (y <number>)) (ra-map #f + x (make-ra y)))


; -----------------------
; lazy ops
; -----------------------

#|
Normal arrays have bounds (lo hi) and an affine map N to 1: [inc ... zero] (a row)
A 'general' array (expr) would have bounds (lo hi) and an affine map M×N to N: [inc₀ ... zero₀; ... incₘ₋₁ ... zeroₘ₋₁] (m rows).
Although if eventually all leaves are 1D, do we need to make that explicit?
So idea 1) for exprs is (op args) where each arg may be an array or an expr.
Then any shape op on arrays needs to be beatable on the args to avoid having to store a full affine map for the expr.
|#


; -----------------------
; ra-rotate!
; -----------------------

(time (ra-rotate! 11 (ra-copy #t (ra-i 4000 3500)))) ; 2.73 - 0.73 with ra-swap!


; -----------------------
; ra-amend!
; -----------------------

(ra-from (ra-copy #t (ra-i 6 4))
         (array->ra #2((0 1) (2 3) (4 5))) (array->ra #1(3 2 1)))
(ra-amend! (ra-copy #t (ra-i 6 4)) (array->ra #(A B C))
           (array->ra #2((0 1) (2 3) (4 5))) (array->ra #1(3 2 1)))
(ra-amend! (ra-copy #t (ra-i 6 4)) (array->ra #2((A a) (B b) (C c)))
           (array->ra #2((0 1) (2 3) (4 5))) (array->ra #1(3 2 1)))
(ra-amend! (ra-copy #t (ra-i 6 4)) (array->ra #3(((A B C) (a b c)) ((P Q R) (p q r)) ((X Y Z) (x y z))))
           (array->ra #2((0 1) (2 3) (4 5))) (array->ra #1(3 2 1)))


; -----------------------
; can't remember
; -----------------------

(define ra0 (array->ra #(1 2 3)))
(define ra1 (array->ra #@1(1 2 3)))
(define ra2 (array->ra #2((1 2) (3 4))))
(define ra3 (array->ra #2@1@1((1 2) (3 4))))
(define ra4 (array->ra #3@1@1@-1(((1 2 3) (3 4 5)) ((4 5 6) (6 7 8)))))
(define ra5 (array->ra #0(99)))

(define v #(1 2 3 4))

(define (vector->list-forward v)
  (case (vector-length v)
    ((0) '())
    ((1) (list (vector-ref v 0)))
    (else
     (let ((first (list (vector-ref v 0))))
       (let loop ((last first)  (i 1))
         (if (= i (vector-length v))
           first
           (let ((next (list (vector-ref v i))))
             (set-cdr! last next)
             (loop next (+ i 1)))))))))


,m (newra)

; call macro with PARAM according to values OPT of TAG
(define-syntax %tag-dispatch
  (syntax-rules ()
    ((_ tag macro (opt ...) (param ...) args ...)
     (case tag ((opt) (macro param args ...)) ... (else (throw 'bad-tag tag))))))

(%tag-dispatch 'TWO display (ONE TWO) ('one 'two))

(import (srfi 1))

(define-inlinable-case demo1
  (case-lambda
   "DOC"
   (() 0)
   ((a) a)
   ((a b) (+ a b))
   (x (fold + 0 x))))

(define-inlinable-case demo2
  (case-lambda
   (() 0)
   ((a) a)
   ((a b) (+ a b))
   (x (fold + 0 x))))


; -----------------------------
; some examples, benchmarks
; -----------------------------

; GEMM

; guile-newra - pure scheme, Guile 2.9

(define n 100)
(define B (ra-copy #t (ra-i n n)))
(define C (ra-copy #t (ra-i n n)))

; Guile newra doesn't define gemm so we make it up on the spot. A few versions...

,time (define A0 (let* ((A (make-typed-ra #t 0 n n))
                        (X (⍉ A 0 2)))
                   (ra-map! X (lambda (a b c) (+ a (* b c))) X B (⍉ C 1))
                   A))
,time (define A1 (let* ((A (make-typed-ra #t 0 n n))
                        (X (⍉ A 0 2)))
                   (ra-slice-for-each 3 (lambda (a b c) (set! (a) (+ (a) (* (b) (c))))) X B (⍉ C 1))
                   A))
,time (define A2 (let* ((A (make-typed-ra #t 0 n n)))
                   (ra-slice-for-each 2
                     (lambda (A B C) (set! (A) (ra-fold (lambda (a b c) (+ (* b c) a)) 0 B C)))
                     A (⍉ B 0 2) (⍉ C 2 1))
                   A))

; check result
(ra-equal? A0 A1 A2)
(ra-fold + 0 A0)
250032502500000 ; n 100
3205173202000000 ; n 200

(define B (i/t. 'f64 100 100))
(define C (i/t. 'f64 100 100))

,time (define A (blas-dgemm B C 1 'no 'no))
;; 0.015948s real time, 0.029959s run time.  0.020600s spent in GC.

; check result
(over + (ravel A))
2.500325025e14

(define x (ra-copy 'f64 (ra-iota #e1e6)))
,time (ra-fold + 0 x)
,time (let ((a 0.)) (ra-for-each (lambda (x) (set! a (+ a x))) x) a)
,time (let ((a 0.)) (ra-slice-for-each 1 (lambda (x) (set! a (+ a (x)))) x) a)

(define N #e1e7)
(define y (ra-copy 'f64 (ra-iota N)))
,time (ra-fold + 0 y)    ;  ~ 4.7s for #e1e8, 1.7s for #e1e7


(define y (let ((y (make-typed-array 'f64 0 N)))
            (array-index-map! y (lambda (i) i))
            y))

; if you look at the disassembly of these, it looks a lot better than (ra-fold + 0 x). Why is that?

,time (let loop ((a 0) (i 0)) (if (= i N) a (loop (+ a (f64vector-ref y i)) (+ 1 i))))
,time ((lambda (op) (let loop ((a 0) (i 0)) (if (= i N) a (loop (op a (f64vector-ref y i)) (+ 1 i))))) +)
,time (let ((a 0)) (let loop ((i 0)) (if (= i N) a (begin (set! a (+ a (f64vector-ref y i))) (loop (+ 1 i))))))

(let-syntax ((%macro (syntax-rules () ((_  a) a)))) (define (fun a) (%macro a)))
