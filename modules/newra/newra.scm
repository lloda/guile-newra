
; Replacement for Guile C-based array system - Main types
; (c) Daniel Llorens - 2016-2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(define-module (newra newra)
  #:export (make-ra ra? ra-data ra-zero ra-dims ra-vlen ra-vref ra-vset!
            make-dim dim? dim-len dim-lo dim-step dim-ref
            ra-rank ra-type ra-lowest-index array->ra))

(import (srfi srfi-9) (srfi srfi-9 gnu) (srfi srfi-1) (srfi srfi-8)
        (srfi srfi-4 gnu) (srfi srfi-26)
        (only (rnrs base) vector-map vector-for-each))

(define (struct-length s)
  (* 1/2 (string-length (symbol->string (struct-layout s)))))

(define (struct->vector s)
  (let* ((v (make-vector (struct-length s))))
    (array-index-map! v (lambda (i) (struct-ref s i)))
    v))

; ----------------
; dimension record
; ----------------

(define-immutable-record-type <dim>
  (make-dim* size lo step) dim?
  (size dim-len)
  (lo dim-lo)
  (step dim-step))

(define make-dim
  (case-lambda
   ((size) (make-dim* size 0 1))
   ((size lo) (make-dim* size lo 1))
   ((size lo step) (make-dim* size lo step))))

(define (dim-end dim)
  (+ (dim-lo dim) (dim-len dim)))

(define (dim-ref dim i)
  (unless (and (<= (dim-lo dim) i) (< i (dim-end dim)))
    (throw 'dim-ref-out-of-range dim i))
  (+ (dim-lo dim) (* (dim-step dim) i)))

; ----------------
; array type
; ----------------

(define <ra-vtable>
  (let ((v (make-struct/no-tail
            <applicable-struct-vtable>
            (make-struct-layout (string-append "pw" "pwpwpwpwpwpw")))))
    v))

(define (ra? o)
  (and (struct? o) (eq? <ra-vtable> (struct-vtable o))))

(define (make-ra* data zero dims vlen vref vset!)
  (make-struct/no-tail
   <ra-vtable>
   (lambda i i)
   data zero dims vlen vref vset!))

(define (ra-data a)  (unless (ra? a) (throw 'bad-ra a)) (struct-ref a 1))
(define (ra-zero a)  (unless (ra? a) (throw 'bad-ra a)) (struct-ref a 2))
(define (ra-dims a)  (unless (ra? a) (throw 'bad-ra a)) (struct-ref a 3))
(define (ra-vlen a)  (unless (ra? a) (throw 'bad-ra a)) (struct-ref a 4))
(define (ra-vref a)  (unless (ra? a) (throw 'bad-ra a)) (struct-ref a 5))
(define (ra-vset! a) (unless (ra? a) (throw 'bad-ra a)) (struct-ref a 6))

(define (pick-typed-vector-functions v)
  (cond ((vector? v)    (values vector-length    vector-ref    vector-set!   ))
        ((c64vector? v) (values c64vector-length c64vector-ref c64vector-set!))
        ((c32vector? v) (values c32vector-length c32vector-ref c32vector-set!))
        ((f64vector? v) (values f64vector-length f64vector-ref f64vector-set!))
        ((f32vector? v) (values f32vector-length f32vector-ref f32vector-set!))
        ((s64vector? v) (values s64vector-length s64vector-ref s64vector-set!))
        ((s32vector? v) (values s32vector-length s32vector-ref s32vector-set!))
        ((s16vector? v) (values s16vector-length s16vector-ref s16vector-set!))
        ((s8vector? v)  (values s8vector-length  s8vector-ref  s8vector-set! ))
        ((s64vector? v) (values s64vector-length s64vector-ref s64vector-set!))
        ((s32vector? v) (values s32vector-length s32vector-ref s32vector-set!))
        ((s16vector? v) (values s16vector-length s16vector-ref s16vector-set!))
        ((s8vector? v)  (values s8vector-length  s8vector-ref  s8vector-set! ))
        ((string? v)    (values string-length    string-ref    string-set!   ))
        ((bitvector? v) (values bitvector-length bitvector-ref bitvector-set!))
; @TODO extend this idea to 'non-strict arrays' (cf Racket), to a method for drag-along
        ((dim? v)       (values  dim-len         dim-ref       (cut throw 'noo-dim-set! <...>)))
        (else (throw 'bad-ra-data-type v))))

(define (make-ra data zero dims)
  (unless (vector? dims) (throw 'bad-dims dims))
  (vector-for-each (lambda (dim) (unless (dim? dim) (throw 'bad-dim dim))) dims)
; after check
  (receive (vlen vref vset!) (pick-typed-vector-functions data)
    (make-ra* data zero dims vlen vref vset!)))

; for some types of loops, or to transition from Guile C arrays.
(define (ra-lowest-index zero dims)
  (let loop ((b zero) (p (- (vector-length dims) 1)))
    (if (negative? p)
      b
      (let ((dim (vector-ref dims p)))
        (loop (+ b (* (dim-lo dim) (dim-step dim))) (- p 1))))))

; ----------------
; derived functions
; ----------------

(define (ra-rank a)
  (vector-length (ra-dims a)))

(define (ra-type a)
  (let ((v (ra-data a)))
    (cond ((vector? v)    #t)
          ((c64vector? v) 'c64)
          ((c32vector? v) 'c32)
          ((f64vector? v) 'f64)
          ((f32vector? v) 'f32)
          ((s64vector? v) 's64)
          ((s32vector? v) 's32)
          ((s16vector? v) 's16)
          ((s8vector? v)  's8)
          ((s64vector? v) 's64)
          ((s32vector? v) 's32)
          ((s16vector? v) 's16)
          ((s8vector? v)  's8)
          ((string? v)    'a)
          ((bitvector? v) 'b)
; @TODO extend this idea to 'non-strict arrays', to a method for drag-along
          ((dim? v)       'd)
          (else (throw 'bad-ra-data-type v)))))

; FIXME while we work on our constructors, then stop using out of tests.
(define (array->ra a)
  (let ((dims (list->vector
               (map (lambda (b i)
                      (make-dim (- (cadr b) (car b) -1) (car b) i))
                    (array-shape a)
                    (shared-array-increments a)))))
    (make-ra (shared-array-root a)
             (- (shared-array-offset a) (ra-lowest-index 0 dims))
             dims)))
