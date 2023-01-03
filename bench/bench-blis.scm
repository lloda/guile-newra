; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Benchmark basic ops vs guile-ffi-blis. Obviously that's required.

(import (newra) (ice-9 match) (ffi blis) (only (newra tools) repeat))


; -----------------------
; bench vs ffi blis shows one or all of these
; * need for (better) loop order heuristic in %slice-loop etc
; * need for strided rank-1 or rank-2 basic ops
; -----------------------

(define (bench-copym m n)
  (define A (make-typed-array 'f64 1 m n))
  (define At (transpose-array A 1 0))
  (define B (make-typed-array 'f64 1 m n))
  (define Bt (transpose-array B 1 0))
  (define Ct (make-typed-array 'f64 1 n m))
  (define C (transpose-array Ct 1 0))

  (define rA (array->ra A))
  (define rAt (array->ra At))
  (define rB (array->ra B))
  (define rBt (array->ra Bt))
  (define rCt (array->ra Ct))
  (define rC (array->ra C))

  (define rp (ceiling (/ 5000000 (* m n))))

  (define discard (const #f))
  (define bench
    `((,(time (repeat rp (array-copy! A B)))
       ,(time (repeat rp (discard (ra-copy! rB rA))))
       ,(time (repeat rp (discard (blis-dcopym! 0 BLIS-NONUNIT-DIAG BLIS-DENSE BLIS-NO-TRANSPOSE A B)))))
      (,(time (repeat rp (array-copy! At Bt)))
       ,(time (repeat rp (discard (ra-copy! rBt rAt))))
       ,(time (repeat rp (discard (blis-dcopym! 0 BLIS-NONUNIT-DIAG BLIS-DENSE BLIS-NO-TRANSPOSE At Bt)))))
      (,(time (repeat rp (array-copy! A C)))
       ,(time (repeat rp (discard (ra-copy! rC rA))))
       ,(time (repeat rp (discard (blis-dcopym! 0 BLIS-NONUNIT-DIAG BLIS-DENSE BLIS-NO-TRANSPOSE A C)))))))

  (let* ((b (list->ra 2 bench))
         (ref (ra-fold max -inf.0 b))
         (b (ra-map #f / (make-ra ref) b)))
    (format #t "ref/t m ~a n ~a for ref ≈ ~4,3f s.\n" m n ref)
    (ra-format (ra-cats #f 1 (list->ra 1 '(guile newra blis)) b)
               #:compact? #t #:prefix? #f
               #:fmt (lambda (x) (if (real? x) (format #f "~5,1f" x) (format #f "~a" x))))))

(for-each (match-lambda ((m n) (bench-copym m n)))
  '((10 5)
    (100 50)
    (1000 500)
    (10000 5000)
    (100000 50)
    (10 500000)))

(define (bench-setm m n)
  (define A (make-typed-array 'f64 1 m n))
  (define At (transpose-array A 1 0))
  (define Bt (make-typed-array 'f64 1 n m))
  (define B (transpose-array Bt 1 0))

  (define rA (array->ra A))
  (define rAt (array->ra At))
  (define rB (array->ra B))
  (define rBt (array->ra Bt))

  (define discard (const #f))
  (define rp (ceiling (/ 5000000 (* m n))))

  (define fill 3.)
  (define bench
    `((,(time (repeat rp (array-fill! A fill)))
       ,(time (repeat rp (discard (ra-fill! rA fill))))
       ,(time (repeat rp (discard (blis-dsetm! BLIS-NO-CONJUGATE 0 BLIS-NONUNIT-DIAG BLIS-DENSE fill A)))))
      (,(time (repeat rp (array-fill! B fill)))
       ,(time (repeat rp (discard (ra-fill! rB fill))))
       ,(time (repeat rp (discard (blis-dsetm! BLIS-NO-CONJUGATE 0 BLIS-NONUNIT-DIAG BLIS-DENSE fill B)))))))

  (let* ((b (list->ra 2 bench))
         (ref (ra-fold max -inf.0 b))
         (b (ra-map #f / (make-ra ref) b)))
    (format #t "ref/t m ~a n ~a for ref ≈ ~4,3f s.\n" m n ref)
    (ra-format (ra-cats #f 1 (list->ra 1 '(guile newra blis)) b)
               #:compact? #t #:prefix? #f
               #:fmt (lambda (x) (if (real? x) (format #f "~5,1f" x) (format #f "~a" x))))))

(for-each (match-lambda ((m n) (bench-setm m n)))
  '((10 5)
    (100 50)
    (1000 500)
    (10000 5000)
    (100000 50)
    (10 500000)))
