; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Fortran FFI featuring newra types
;;; Code:

(define-module (newra ffi)
  #:export (ra->fortran fortran-library-function))

(import (srfi srfi-8) (srfi srfi-26) (srfi srfi-71) (srfi srfi-1) (srfi srfi-4 gnu)
        (ice-9 match) (ice-9 format) (rnrs bytevectors)
        (system foreign) (system foreign-library)
        (newra) (newra base))

; https://gcc.gnu.org/onlinedocs/gfortran/Further-Interoperability-of-Fortran-with-C.html
; https://github.com/gcc-mirror/gcc/blob/master/libgfortran/ISO_Fortran_binding.h

#|
/* CFI type definitions. */
typedef ptrdiff_t CFI_index_t;
typedef int8_t CFI_rank_t;
typedef int8_t CFI_attribute_t;
typedef int16_t CFI_type_t;

/* CFI_dim_t. */
typedef struct CFI_dim_t
  {
    CFI_index_t lower_bound;
    CFI_index_t extent;
    CFI_index_t sm; // step between start of successive elements, in bytes
  }
CFI_dim_t;

/* CFI_cdesc_t, C descriptors are cast to this structure as follows:
   CFI_CDESC_T(CFI_MAX_RANK) foo;
   CFI_cdesc_t * bar = (CFI_cdesc_t *) &foo;
 */
typedef struct CFI_cdesc_t
 {
    void *base_addr;
    size_t elem_len; // size of one element, in bytes
    int version;
    CFI_rank_t rank;
    CFI_attribute_t attribute;
    CFI_type_t type;
    CFI_dim_t dim[];
 }
CFI_cdesc

#define CFI_type_Integer 1
#define CFI_type_Logical 2
#define CFI_type_Real 3
#define CFI_type_Complex 4
#define CFI_type_Character 5
|#

(define CFI_VERSION 1)
(define CFI_MAX_RANK 15)

(define CFI_attribute_pointer 0)
(define CFI_attribute_allocatable 1)
(define CFI_attribute_other 2)

(define CFI_type_Integer 1)
(define CFI_type_Logical 2)
(define CFI_type_Real 3)
(define CFI_type_Complex 4)
(define CFI_type_Character 5)

(define CFI_type_kind_shift 8)

(define CFI_type_int8_t (+ CFI_type_Integer (ash (sizeof int8) CFI_type_kind_shift)))
(define CFI_type_int16_t (+ CFI_type_Integer (ash (sizeof int16) CFI_type_kind_shift)))
(define CFI_type_int32_t (+ CFI_type_Integer (ash (sizeof int32) CFI_type_kind_shift)))
(define CFI_type_int64_t (+ CFI_type_Integer (ash (sizeof int64) CFI_type_kind_shift)))
(define CFI_type_float (+ CFI_type_Real (ash (sizeof float) CFI_type_kind_shift)))
(define CFI_type_double (+ CFI_type_Real (ash (sizeof double) CFI_type_kind_shift)))
(define CFI_type_float_Complex (+ CFI_type_Complex (ash (sizeof float) CFI_type_kind_shift)))
(define CFI_type_double_Complex (+ CFI_type_Complex (ash (sizeof double) CFI_type_kind_shift)))

(define (CFI-type t)
  (case t
    ((vu8 u8 s8) CFI_type_int8_t)
    ((u16 s16) CFI_type_int16_t)
    ((u32 s32) CFI_type_int32_t)
    ((u64 s64) CFI_type_int64_t)
    ((f32) CFI_type_float)
    ((f64) CFI_type_double)
    ((c32) CFI_type_float_Complex)
    ((c64) CFI_type_double_Complex)
    (else (throw 'no-CFI-type-for t))))

(define CFI_index_t ptrdiff_t)
(define CFI_rank_t int8)
(define CFI_attribute_t int8)
(define CFI_type_t int16)

(define (ra->fortran a)
  (define theversion 1)
  (define theattribute CFI_attribute_pointer)
  (let ((rank (ra-rank a))
        (elemsize (srfi-4-vector-type-size (ra-root a))))
    (unless (<= 0 rank CFI_MAX_RANK)
      (throw 'bad-rank rank))
    (make-c-struct
     (append (list '*                            ; 0
                   size_t                        ; 8
                   int                           ; 16
                   CFI_rank_t                    ; 20
                   CFI_attribute_t               ; 21
                   CFI_type_t)                   ; 22
             (make-list (* 3 rank) CFI_index_t)) ; 24
     (append (list (bytevector->pointer (ra-root a) (* elemsize (ra-offset a))) ; 0
                   elemsize                              ; 8
                   theversion                            ; 16
                   rank                                  ; 20
                   theattribute                          ; 21
                   (CFI-type (ra-type a)))               ; 22
             (append-map                                 ; 24
              (match-lambda
                (($ <dim> len lo step)
; lbound is always 1 fortranside. FIXME warn?
                 (list lo len (* elemsize step))))
              (vector->list (ra-dims a)))))))

(define (ra->ffi-type t)
  (case t
    ((vu8 u8) uint8)
    ((u16) uint16)
    ((u32) uint32)
    ((u64) uint64)
    ((s8) int8)
    ((s16) int16)
    ((s32) int32)
    ((s64) int64)
    ((f32) float)
    ((f64) double)
    ((c32) complex-float)
    ((c64) complex-double)
    (else (throw 'no-ffi-type-for t))))

(define (symbol->ffi-type t)
  (case t
    ((uint8) uint8)
    ((uint16) uint16)
    ((uint32) uint32)
    ((uint64) uint64)
    ((int8) int8)
    ((int16) int16)
    ((int32) int32)
    ((int64) int64)
    ((float) float)
    ((double) double)
    ((complex-float) complex-float)
    ((complex-double) complex-double)
    (else (throw 'no-ffi-type-for t))))

(define* (fortran-library-function lib name return-type arg-types)
  (let ((f (foreign-library-function
            lib name #:return-type return-type
            #:arg-types (make-list (length arg-types) '*))))
    (lambda args
      (let ((fargs
             (map (match-lambda*
                    ((arg (type-symbol dims ...))
                     (unless (eqv? (symbol->ffi-type type-symbol) (ra->ffi-type (ra-type arg)))
                       (throw 'bad-type type-symbol (ra-type arg)))
                     (ra->fortran
                      (let ((ndims (length dims)))
                        (if (and (= ndims 1) (eq? '.. (car dims)))
                          arg
                          (if (and (= ndims (ra-rank arg))
                                   (every (lambda (lohi dim)
                                            (or (eq? ': dim) (equal? lohi dim)))
                                          (ra-dimensions arg) dims))
                            arg
                            (throw 'bad-sizes (ra-dimensions arg) dims))))))
                    ((arg '*)
                     arg)
                    ((arg type-symbol)
                     (make-c-struct (list (symbol->ffi-type type-symbol)) (list arg))))
               args
               arg-types)))
      (apply f fargs)))))
