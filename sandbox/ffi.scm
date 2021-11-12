; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Fortran FFI sandbox

(import (newra) (newra base) (newra ffi)
        (srfi 8) (srfi 26) (srfi 71) (srfi 1) (srfi 111)
        (ice-9 match) (ice-9 format)
        (rnrs bytevectors)
        (system foreign) (system foreign-library))

(define libexample (load-foreign-library "./libexample"))
(define lookup-xy (foreign-library-function libexample "lookup_xy" #:return-type double #:arg-types '(* * *)))
(define ranker (foreign-library-function libexample "ranker" #:return-type int32 #:arg-types '(*)))
(define valuer (foreign-library-function libexample "valuer" #:return-type double #:arg-types '(*)))
(define lbounder (foreign-library-function libexample "lbounder" #:return-type int32 #:arg-types '(*)))

(define (make-1 type x) (make-c-struct (list type) (list x)))

(define x 1.)
(define y 2.)

(define table0 (ra-copy 'f64 (ra-i 3 4)))

(define table1
  (let* ((bigtable (make-typed-ra 'f64 0. 10 10))
         (table (ra-from bigtable (ra-iota 3 2) (ra-iota 4 3))))
    (ra-copy! table (ra-i 3 4))
    table))

(define table2 (ra-reshape (ra-reshape (ra-copy 'f64 (ra-i 3 4)) 0 '(2 4)) 1 '(2 5)))

(lookup-xy (make-1 double 1) (make-1 double 2) (ra->fortran table0)) ; 6.
(lookup-xy (make-1 double 1) (make-1 double 2) (ra->fortran table1)) ; 6.
(lookup-xy (make-1 double 1) (make-1 double 2) (ra->fortran table2)) ; 6. :-\
(ranker (ra->fortran (make-typed-ra 'f64 0))) ; 0
(ranker (ra->fortran (make-typed-ra 'f64 0 2 2))) ; 2
(ranker (ra->fortran (make-typed-ra 'f64 0 1))) ; 1
(valuer (ra->fortran (make-typed-ra 'f64 7))) ; 7
(valuer (ra->fortran (make-typed-ra 'f64 7 1))) ; 99
(lbounder (ra->fortran (make-typed-ra 'f64 1. 3))) ; 1
(lbounder (ra->fortran (ra-reshape (make-typed-ra 'f64 1. 3) 0 '(3 5)))) ; 1 :-\

(define lookup-xy* (fortran-library-function libexample "lookup_xy" double '(double double (double : :))))
(define ranker* (fortran-library-function libexample "ranker" int32 '((double ..))))
(define valuer* (fortran-library-function libexample "valuer" double '((double ..))))
(define lbounder* (fortran-library-function libexample "lbounder" int32 '((double :))))
(define lookup-xy-complex* (fortran-library-function libexample "lookup_xy_complex" complex-double '(double double (complex-double : :))))
(define conjg* (fortran-library-function libexample "conjugate" complex-double '(complex-double)))

(lookup-xy* 1 2 table0) ; 6.
(lookup-xy* 1 2 table1) ; 6.
(lookup-xy* 1 2 table2) ; 6. :-\
(lookup-xy-complex* 1 2 (ra-copy 'c64 table0)) ; 6.0 + 0.0i
(lookup-xy-complex* 1 2 (ra-map 'c64 (cut make-rectangular 0 <>) table0)) ; 0.0+6.0i
(lookup-xy-complex* 1 2 (ra-map 'c64 (lambda (x) (make-rectangular x (* x .5))) table0)) ; 6.0+3.0i
(ranker* (make-typed-ra 'f64 0)) ; 0
(ranker* (make-typed-ra 'f64 0 2 2)) ; 2
(ranker* (make-typed-ra 'f64 0 1)) ; 1
(valuer* (make-typed-ra 'f64 7)) ; 7
(valuer* (make-typed-ra 'f64 7 1)) ; 99
(lbounder* (make-typed-ra 'f64 1. 3)) ; 1
(lbounder* (ra-reshape (make-typed-ra 'f64 1. 3) 0 '(3 5))) ; 1 :-\
(conjg* 3+9i) ; 3.0-9.0i

; FIXME not sure how it works for intent(out) args. It seems Fortran allocates it, so we need to recover it from the pointer somehow? and who frees it?

(define dgemv* (fortran-library-function libexample "dgemv" void '((double : :) (double :) *)))

(let ((a (ra-copy 'f64 (ra-i 4 3)))
      (v (make-ra-root #f64(1 2 3)))
      (w (make-c-struct (list 3) (list double))))
  (dgemv* a v w)
  w)

#|
[x] fix rank 0
[x] fix non-zero ra-offset
[x] verify behavior of lbounds
[ ]fortran-library-function
  [x] c32/c64
  [x] fix arg-types format
  [ ] support intent(out) array args
  [ ] bool
  [ ] support out args, e.g. with boxes (?)
[ ] fix alignment assumptions (seems there's padding in some versions of ISO_Fortran_binding.h :-/)
[ ] modulize, doc, etc.
|#
