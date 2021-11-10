; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Fortran FFI sandbox

(import (newra) (newra base) (newra ffi)
        (srfi 8) (srfi 26) (srfi 71) (srfi 1)
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

#|
[x] fix rank 0
[x] fix non-zero ra-offset
[x] verify behavior of lbounds
[ ] define something like fortran-library-function
[ ] support in/out/inout
[ ] fix alignment assumptions (seems there's padding in some versions of ISO_Fortran_binding.h :-/)
[ ] modulize, doc, etc.
|#
