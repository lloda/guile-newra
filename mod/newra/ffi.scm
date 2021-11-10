; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; FFI featuring newra types
;;; Code:

(define-module (newra ffi)
  #:export (ra->fortran))

(import (srfi 8) (srfi 26) (srfi 71) (srfi 1) (ice-9 match) (ice-9 format) (rnrs bytevectors)
        (system foreign) (system foreign-library)
        (newra) (newra base))

; https://github.com/gcc-mirror/gcc/blob/master/libgfortran/ISO_Fortran_binding.h
; https://www.ibm.com/docs/en/xl-fortran-aix/16.1.0?topic=29113-type-definitions-structures
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
        (elemsize (bytevector-type-size (ra-type a))))
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
              (lambda (dim)
; lbound is always 1 fortranside. FIXME warn?
                (list (dim-lo dim)
                      (dim-len dim)
                      (* elemsize (dim-step dim))))
              (vector->list (ra-dims a)))))))
