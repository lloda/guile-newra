; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Fortran FFI sandbox
; see ffi.cc, https://j3-fortran.org/doc/year/18/18-007r1.pdf

(import (newra) (newra base) (newra ffi)
        (srfi srfi-8) (srfi srfi-26) (srfi srfi-71) (srfi srfi-1) (srfi srfi-111)
        (ice-9 match) (ice-9 format) (rnrs bytevectors)
        (system foreign) (system foreign-library))

(define libexample (load-foreign-library "./libexample"))
(define lookup-xy (fortran-library-function libexample "lookup_xy" double '(double double (double : :))))
(define ranker (fortran-library-function libexample "ranker" int32 '((double ..))))
(define valuer (fortran-library-function libexample "valuer" double '((double ..))))
(define lbounder (fortran-library-function libexample "lbounder" int32 '((double :))))
(define lookup-xy-complex (fortran-library-function libexample "lookup_xy_complex" complex-double '(double double (complex-double : :))))
(define conjg (fortran-library-function libexample "conjugate" complex-double '(complex-double)))

(define x 1.)
(define y 2.)

(define table0 (ra-copy 'f64 (ra-i 3 4)))

(define table1
  (let* ((bigtable (make-typed-ra 'f64 0. 10 10))
         (table (ra-from bigtable (ra-iota 3 2) (ra-iota 4 3))))
    (ra-copy! table (ra-i 3 4))
    table))

(define table2 (ra-reshape (ra-reshape (ra-copy 'f64 (ra-i 3 4)) 0 '(2 4)) 1 '(2 5)))

(lookup-xy 1 2 table0) ; 6.
(lookup-xy 1 2 table1) ; 6.
(lookup-xy 1 2 table2) ; 6. :-\
(lookup-xy-complex 1 2 (ra-copy 'c64 table0)) ; 6.0 + 0.0i
(lookup-xy-complex 1 2 (ra-map 'c64 (cut make-rectangular 0 <>) table0)) ; 0.0+6.0i
(lookup-xy-complex 1 2 (ra-map 'c64 (lambda (x) (make-rectangular x (* x .5))) table0)) ; 6.0+3.0i
(ranker (make-typed-ra 'f64 0)) ; 0
(ranker (make-typed-ra 'f64 0 2 2)) ; 2
(ranker (make-typed-ra 'f64 0 1)) ; 1
(valuer (make-typed-ra 'f64 7)) ; 7
(valuer (make-typed-ra 'f64 7 1)) ; 99
(lbounder (make-typed-ra 'f64 1. 3)) ; 1
(lbounder (ra-reshape (make-typed-ra 'f64 1. 3) 0 '(3 5))) ; 1 :-\
(conjg 3+9i) ; 3.0-9.0i

; inout array

(define fillerf32 (fortran-library-function libexample "fillerf32" void '((float :))))
(define fillerf64 (fortran-library-function libexample "fillerf64" void '((double :))))

(let* ((a (ra-copy (make-ra-root #f32(1 2 3 4 5 6 7 8))))
       (b (ra-from a (ra-iota 4 2))))
  (fillerf32 b)
  (display a) (newline)) ; #%1f32:8(1.0 2.0 9.0 16.0 25.0 36.0 7.0 8.0)

(let* ((a (ra-copy (make-ra-root #f64(1 2 3 4 5 6 7 8))))
       (b (ra-from a (ra-iota 4 2))))
  (fillerf64 b)
  (display a) (newline)) ; #%1f64:8(1.0 2.0 9.0 16.0 25.0 36.0 7.0 8.0)

(define dgemv (fortran-library-function libexample "dgemv" int8 '((double : :) (double :) (double :))))

(let ((a (ra-copy 'f64 (ra-i 4 3)))
      (v (make-ra-root #f64(1 2 3)))
      (w (ra-copy (make-ra-root #f64(9 8 7 6)))))
  (unless (zero? (dgemv a v w)) (throw 'error))
  (display w) (newline) ; #%1f64:4(8.0 26.0 44.0 62.0)
  w)
