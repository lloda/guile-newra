
(import (srfi srfi-9) (srfi srfi-9 gnu) (srfi srfi-1) (srfi srfi-8) (srfi srfi-4 gnu))

(define-immutable-record-type <dim>
  (make-dim size lo step) dim?
  (size dim-size)
  (lo dim-lo)
  (step dim-step))

(define (dim-end dim)
  (+ (dim-lo dim) (dim-size dim)))

(define-immutable-record-type <ra>
  (make-ra* data vref vset! base dims) ra?
  (data ra-data)
  (vref ra-vref)
  (vset! ra-vset)
  (base ra-base)
  (dims ra-dims))

(define (pick-typed-vector-accessors v)
  (cond ((c64vector? v) (values c64vector-ref c64vector-set!))
        ((c32vector? v) (values c32vector-ref c32vector-set!))
        ((f64vector? v) (values f64vector-ref f64vector-set!))
        ((f32vector? v) (values f32vector-ref f32vector-set!))
        ((s64vector? v) (values s64vector-ref s64vector-set!))
        ((s32vector? v) (values s32vector-ref s32vector-set!))
        ((s16vector? v) (values s16vector-ref s16vector-set!))
        ((s8vector? v)  (values s8vector-ref  s8vector-set!))
        ((s64vector? v) (values s64vector-ref s64vector-set!))
        ((s32vector? v) (values s32vector-ref s32vector-set!))
        ((s16vector? v) (values s16vector-ref s16vector-set!))
        ((s8vector? v)  (values s8vector-ref  s8vector-set!))
        ((vector? v)    (values vector-ref     vector-set!))
        (else (throw 'bad-vector-base-type v))))

(define (make-ra data base dims)
  (receive (vref vset!) (pick-typed-vector-accessors data)
    (make-ra* data vref vset! base dims)))

(define (ra-rank ra)
  (length (ra-dims ra)))

(define (ra-tally ra)
  (unless (positive? (ra-rank ra))
    (throw 'ra-tally-bad-rank))
  (dim-size (car (ra-dims ra))))

(define (ra-item ra i)
  (let ((dims (ra-dims ra)))
    (if (null? dims)
      (throw 'ra-item-zero-rank)
      (receive (dim0 dims+) (car+cdr dims)
        (unless (and (<= (dim-lo dim0) i)
                     (< i (dim-end dim0)))
          (throw 'ra-item-out-of-range i (dim-lo dim0) (dim-end dim0)))
        (if (null? dims+)
          ((ra-vref ra) (ra-data ra) (+ (ra-base ra) i))
          (set-fields ra
            ((ra-base) (+ (ra-base ra) (* (dim-step dim0) (- i (dim-lo dim0)))))
            ((ra-dims) dims+)))))))

(define (ra-cell ra . i)
  (fold (lambda (i ra) (ra-item ra i)) ra i))

(define ra0 (make-ra #(1 2 3 4 5 6) 0 (list (make-dim 2 0 3) (make-dim 3 0 1))))
(define ra1 (make-ra #(1 2 3 4 5 6) 0 (list (make-dim 3 0 2) (make-dim 2 0 1))))
  
    
    


