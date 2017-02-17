
; Replacement for Guile C-based array system - Main types
; (c) Daniel Llorens - 2016-2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(define-module (newra newra)
  #:export (make-ra ra? ra-data ra-zero ra-dims ra-vlen ra-vref ra-vset!
            make-dim dim? dim-len dim-lo dim-step dim-ref
            ra-rank ra-type
            make-ra-new make-ra-data array->ra
            ra-pos ra-pos-first ra-pos-lo ra-pos-hi
            ra-slice ra-ref ra-cell))

(import (srfi srfi-9) (srfi srfi-9 gnu) (only (srfi srfi-1) fold) (srfi srfi-8)
        (srfi srfi-4 gnu) (srfi srfi-26) (ice-9 match)
        (only (rnrs base) vector-map vector-for-each))

(define (struct-length s)
  (* 1/2 (string-length (symbol->string (struct-layout s)))))

(define (struct->vector s)
  (let* ((v (make-vector (struct-length s))))
    (array-index-map! v (lambda (i) (struct-ref s i)))
    v))

(define vector-fold
  (case-lambda
   ((kons knil)
    (throw 'missing-arguments))
   ((kons knil v)
    (let ((end (vector-length v)))
      (let loop ((i 0) (k knil))
        (if (= i end)
          k
          (loop (+ i 1) (kons (vector-ref v i) k))))))
   ((kons knil . vs)
    (let ((end (vector-length (car vs))))
      (let loop ((i 0) (k knil))
        (if (= i end)
          k
          (loop (+ i 1) (apply kons (append (map (cut vector-ref <> i) vs) (list k))))))))))

; FIXME ev. used newra shared, will be simpler.
(define (vector-clip v lo end)
  (unless (and (<= 0 lo end) (<= end (vector-length v)))
    (throw 'bad-arguments lo end (vector-length v)))
  (let ((w (make-vector (- end lo) *unspecified*)))
    (let loop ((i lo))
      (if (= i end)
        w
        (begin
          (vector-set! w (- i lo) (vector-ref v i))
          (loop (+ i 1)))))))

(define (vector-drop v n)
  (vector-clip v n (vector-length v)))

(define (vector-take v n)
  (vector-clip v 0 n))

; ----------------
; dimension record
; ----------------

(define-immutable-record-type <dim>
  (make-dim* len lo step) dim?
  (len dim-len)
  (lo dim-lo)
  (step dim-step))

(define make-dim
  (case-lambda
   ((len) (make-dim* len 0 1))
   ((len lo) (make-dim* len lo 1))
   ((len lo step) (make-dim* len lo step))))

(define (dim-end dim)
  (+ (dim-lo dim) (dim-len dim)))

(define (dim-ref dim i)
  (unless (and (<= 0 i) (< i (dim-len dim)))
    (throw 'dim-ref-out-of-range dim i))
  (+ (dim-lo dim) (* (dim-step dim) i)))

(define (dim-check dim i)
  (unless (and (<= (dim-lo dim) i) (< i (+ (dim-lo dim) (dim-len dim))))
    (throw 'dim-check-out-of-range dim i))
  i)

; ----------------
; array type
; ----------------

(define <ra-vtable>
  (let ((v (make-struct/no-tail
            <applicable-struct-with-setter-vtable>
            (make-struct-layout (string-append "pwpw" "pwpwpwpwpwpw")))))
    v))

(define (ra? o)
  (and (struct? o) (eq? <ra-vtable> (struct-vtable o))))

(define (make-ra* data zero dims type vlen vref vset!)
  (letrec ((ra
            (make-struct/no-tail
             <ra-vtable>
             (lambda i (apply ra-cell ra i))
             (lambda i (format (current-error-port) "you've called SET! ra (~a) with args: ~a\n" ra i))
             data zero dims type vlen vref vset!)))
    ra))

(define field0 2)
(define (ra-data a)  (unless (ra? a) (throw 'bad-ra a)) (struct-ref a (+ field0 0)))
(define (ra-zero a)  (unless (ra? a) (throw 'bad-ra a)) (struct-ref a (+ field0 1)))
(define (ra-dims a)  (unless (ra? a) (throw 'bad-ra a)) (struct-ref a (+ field0 2)))
(define (ra-type a)  (unless (ra? a) (throw 'bad-ra a)) (struct-ref a (+ field0 3)))
(define (ra-vlen a)  (unless (ra? a) (throw 'bad-ra a)) (struct-ref a (+ field0 4)))
(define (ra-vref a)  (unless (ra? a) (throw 'bad-ra a)) (struct-ref a (+ field0 5)))
(define (ra-vset! a) (unless (ra? a) (throw 'bad-ra a)) (struct-ref a (+ field0 6)))

(define (pick-typed-vector-functions v)
  (cond ((vector? v)    (values  #t    vector-length     vector-ref     vector-set!   ))
        ((c64vector? v) (values  'c64  c64vector-length  c64vector-ref  c64vector-set!))
        ((c32vector? v) (values  'c32  c32vector-length  c32vector-ref  c32vector-set!))
        ((f64vector? v) (values  'f64  f64vector-length  f64vector-ref  f64vector-set!))
        ((f32vector? v) (values  'f32  f32vector-length  f32vector-ref  f32vector-set!))
        ((s64vector? v) (values  's64  s64vector-length  s64vector-ref  s64vector-set!))
        ((s32vector? v) (values  's32  s32vector-length  s32vector-ref  s32vector-set!))
        ((s16vector? v) (values  's16  s16vector-length  s16vector-ref  s16vector-set!))
        ((s8vector? v)  (values  's8   s8vector-length   s8vector-ref   s8vector-set! ))
        ((u64vector? v) (values  'u64  s64vector-length  s64vector-ref  s64vector-set!))
        ((u32vector? v) (values  'u32  s32vector-length  s32vector-ref  s32vector-set!))
        ((u16vector? v) (values  'u16  s16vector-length  s16vector-ref  s16vector-set!))
        ((u8vector? v)  (values  'u8   s8vector-length   s8vector-ref   s8vector-set! ))
        ((string? v)    (values  'a    string-length     string-ref     string-set!   ))
        ((bitvector? v) (values  'b    bitvector-length  bitvector-ref  bitvector-set!))
; @TODO extend this idea to 'non-strict arrays' (cf Racket), to a method for drag-along
        ((dim? v)       (values 'd   dim-len          dim-ref       (cut throw 'no-dim-set! <...>)))
        (else (throw 'bad-ra-data-type v))))

; low level, for conversions
(define (make-ra data zero dims)
  (unless (vector? dims) (throw 'bad-dims dims))
  (vector-for-each (lambda (dim) (unless (dim? dim) (throw 'bad-dim dim))) dims)
; after check
  (receive (type vlen vref vset!) (pick-typed-vector-functions data)
    (make-ra* data zero dims type vlen vref vset!)))

; FIXME we'll optimize these I think
(define (ra-pos zero dims . i_)
  (let loop ((i i_) (j 0) (pos zero))
    (if (null? i)
      pos
      (if (>= j (vector-length dims))
        (throw 'too-many-indices i_)
        (let ((dim (vector-ref dims j)))
          (loop (cdr i) (+ j 1) (+ pos (* (dim-check dim (car i)) (dim-step dim)))))))))

; lowest position on data.
(define (ra-pos-lo ra)
  (let ((dims (ra-dims ra)))
    (let loop ((j (- (vector-length dims) 1)) (pos (ra-zero ra)))
      (if (< j 0)
        pos
        (let* ((dim (vector-ref dims j))
               (step (dim-step dim)))
          (loop (- j 1) (+ pos (* step (+ (dim-lo dim) (if (positive? step) 0 (- (dim-len dim) 1)))))))))))

; highest position on data.
(define (ra-pos-hi ra)
  (let ((dims (ra-dims ra)))
    (let loop ((j (- (vector-length dims) 1)) (pos (ra-zero ra)))
      (if (< j 0)
        pos
        (let* ((dim (vector-ref dims j))
               (step (dim-step dim)))
          (loop (- j 1) (+ pos (* step (+ (dim-lo dim) (if (positive? step) (- (dim-len dim) 1) 0))))))))))

; first position of the array (when all indices = dim-lo)
; useful for some types of loops, or to transition from Guile C arrays.
(define (ra-pos-first zero dims)
  (let loop ((j (- (vector-length dims) 1)) (pos zero))
    (if (< j 0)
      pos
      (let ((dim (vector-ref dims j)))
        (loop (- j 1) (+ pos (* (dim-lo dim) (dim-step dim))))))))

(define (ra-ref ra . i)
  (unless (= (ra-rank ra) (length i))
    (throw 'bad-number-of-indices (ra-rank ra) (length i)))
  ((ra-vref ra) (ra-data ra) (apply ra-pos (ra-zero ra) (ra-dims ra) i)))

(define (ra-slice ra . i)
  (make-ra (ra-data ra)
           (apply ra-pos (ra-zero ra) (ra-dims ra) i)
           (vector-drop (ra-dims ra) (length i))))

(define (ra-cell ra . i)
  (let ((pos (apply ra-pos (ra-zero ra) (ra-dims ra) i)))
    (if (= (ra-rank ra) (length i))
      ((ra-vref ra) (ra-data ra) pos)
      (make-ra (ra-data ra) pos (vector-drop (ra-dims ra) (length i))))))

; ----------------
; derived functions
; ----------------

(define (ra-rank a)
  (vector-length (ra-dims a)))

(define (c-dims-size d)
  (let* ((dims (let loop ((d d))
                 (match d
                   (()
                    '())
                   (((lo hi))
                    (list (make-dim (- hi lo -1) lo 1)))
                   ((len)
                    (list (make-dim len 0 1)))
                   (((lo hi) . rest)
                    (let ((next (loop rest))
                          (len (- hi lo -1)))
                      (cons (make-dim len lo (* (dim-len (car next)) (dim-step (car next)))) next)))
                   ((len . rest)
                    (let ((next (loop rest)))
                      (cons (make-dim len 0 (* (dim-len (car next)) (dim-step (car next)))) next)))))))
    (values (list->vector dims)
            (fold (lambda (a c) (* c (dim-len a))) 1 dims))))

(define (make-ra-data data . d)
  (receive (dims size) (c-dims-size d)
      (make-ra data
               (- (ra-pos-first 0 dims))
               dims)))

(define (make-ra-new type value . d)
  (receive (dims size) (c-dims-size d)
      (make-ra (make-typed-array type value size)
               (- (ra-pos-first 0 dims))
               dims)))

; ----------------
; transition help
; ----------------

(define (array->ra a)
  (let ((dims (list->vector
               (map (lambda (b i)
                      (make-dim (- (cadr b) (car b) -1) (car b) i))
                    (array-shape a)
                    (shared-array-increments a)))))
    (make-ra (shared-array-root a)
             (- (shared-array-offset a) (ra-pos-first 0 dims))
             dims)))

(define (ra->array a)
  'FIXME)
