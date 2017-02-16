
; Replacement for Guile C-based array system - Main types
; (c) Daniel Llorens - 2016-2017

(define-module (newra newra)
  #:export (make-ra ra? ra-rank
            ra-data ra-base ra-dims ra-vlen ra-vref ra-vset!
            dim? dim-size dim-lo dim-step dim-ref))

(import (srfi srfi-9) (srfi srfi-9 gnu) (srfi srfi-1) (srfi srfi-8)
        (srfi srfi-4 gnu) (srfi srfi-26) (rnrs base))

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
  (size dim-size)
  (lo dim-lo)
  (step dim-step))

(define make-dim
  (case-lambda
   ((size) (make-dim* size 0 1))
   ((size lo) (make-dim* size lo 1))
   ((size lo step) (make-dim* size lo step))))

(define (dim-end dim)
  (+ (dim-lo dim) (dim-size dim)))

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
    (struct-set! v vtable-index-printer
                 (lambda (a port)
                   (display "%" port)
                   (display (ra-rank a) port)
                   (display "(…)")))
    v))

(define (ra? o)
  (and (struct? o) (eq? <ra-vtable> (struct-vtable o))))

(define (make-ra* data base dims vlen vref vset!)
  (make-struct/no-tail
   <ra-vtable>
   (lambda i i)
   data base dims vlen vref vset!))

(define (ra-data a) (struct-ref a 1))
(define (ra-base a) (struct-ref a 2))
(define (ra-dims a) (struct-ref a 3))
(define (ra-vlen a) (struct-ref a 4))
(define (ra-vref a) (struct-ref a 5))
(define (ra-vset! a) (struct-ref a 6))

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
; @TODO extend this idea to 'non-strict arrays', to a method for drag-along
        ((dim? v)       (values  dim-size        dim-ref       (cut throw 'noo-dim-set! <...>)))
        (else (throw 'bad-vector-base-type v))))

(define (make-ra data base dims)
  (unless (vector? dims) (throw 'bad-dims dims))
  (vector-for-each (lambda (dim) (unless (dim? dim) (throw 'bad-dim dim))) dims)

  (receive (vlen vref vset!) (pick-typed-vector-functions data)
    (make-ra* data base dims vlen vref vset!)))

(define (ra-rank a)
  (vector-length (ra-dims a)))
