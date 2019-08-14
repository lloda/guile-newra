; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2016-2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Basic definitions for arrays.
;;; Code:

(define-module (newra base)
  #:export (ra?
            make-ra-raw
            ra-root ra-zero ra-zero-set! ra-dims ra-type ra-vlen ra-vref ra-vset!
            check-ra
            ra-rank ra-type make-ra-new make-ra-root
            make-dim dim? dim-len dim-lo dim-hi dim-step dim-ref c-dims
            ra-pos ra-offset ra-pos-hi ra-pos-lo
            ra-slice ra-cell ra-ref ra-set!
; for internal (newra) use, don't re-export
            make-dim* <dim>
            vector-drop vector-fold vector-clip
            <ra-vtable> pick-root-functions pick-make-root
            %ra-root %ra-zero %ra-zero-set! %ra-dims %ra-type %ra-vlen %ra-vref %ra-vset! %ra-rank
            %%ra-root %%ra-zero %%ra-zero-set! %%ra-dims %%ra-type %%ra-vlen %%ra-vref %%ra-vset! %%ra-rank
            %%ra-step))

(import (srfi :9) (srfi srfi-9 gnu) (only (srfi :1) fold every) (srfi :8)
        (srfi srfi-4 gnu) (srfi :26) (srfi :2) (ice-9 match) (ice-9 control)
        (only (rnrs base) vector-for-each))


; ----------------
;; Conventions
; ----------------

;; ra:          an array-type view created by make-ra*
;; dim:         each axis of an ra, or its bounds, as many as the rank.
;; index:       integer as axis argument
;; lo:          lowest index in a dim
;; hi:          highest index in a dim
;; end:         one past hi
;; len:         length of a dim = end-lo
;; lenm:        len - 1.
;; v:           a vector
;; l:           a list
;; i, j:        indices in a dim, from hi to lo
;; k:           an index in a dim vector, from 0 to rank-1
;; slice:       an ra, as a piece of another ra
;; cell:        (also prefix-cell) slice obtained by fixing the first k indices into an ra.
;; item:        slice obtained by fixing the first index into an ra; a (rank - 1)-cell.


; ----------------
; misc - FIXME remove if unused
; ----------------

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
; dimension record, used both as that, and as delayed iota.
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
   ((len lo step)
    (when (and len (< len 0)) (throw 'bad-dim-len len))
    (make-dim* len lo step))))

(define-inlinable (dim-end dim)
  (+ (dim-lo dim) (dim-len dim)))

(define-inlinable (dim-hi dim)
  (let ((len (dim-len dim)))
    (and len (+ (dim-lo dim) (dim-len dim) -1))))

(define-inlinable (dim-ref dim i)
  (and-let* ((len (dim-len dim)))
    (unless (and (<= 0 i) (< i len))
      (throw 'dim-ref-out-of-range dim i)))
  (+ (dim-lo dim) (* (dim-step dim) i)))

(define-inlinable (dim-check dim i)
  (let ((len (dim-len dim))
        (lo (dim-lo dim)))
    (if (and (>= i lo) (or (not len) (< i (+ len lo))))
      i
      (throw 'dim-check-out-of-range dim i))))


; ----------------
; the array/view type
; ----------------

; fields are: [apply setter data zero dims type vlen vref vset!]
(define <ra-vtable>
  (make-struct/no-tail
   <applicable-struct-with-setter-vtable>
   (make-struct-layout "pwpwpwpwpwpwpwpwpw")))

(define-inlinable (ra? o)
  (and (struct? o) (eq? <ra-vtable> (struct-vtable o))))

(define-inlinable (check-ra o)
  (if (ra? o) o (throw 'not-ra? o)))

(define (make-ra* data zero dims type vlen vref vset!)
  (letrec ((ra
            (make-struct/no-tail ; FIXME use /simple on Guile 3.
             <ra-vtable>
             (case-lambda
               (() (ra-cell ra))
               ((i0) (ra-cell ra i0))
               ((i0 i1) (ra-cell ra i0 i1))
               ((i0 i1 i2) (ra-cell ra i0 i1 i2))
               ((i0 i1 i2 i3) (ra-cell ra i0 i1 i2 i3))
               (i (apply ra-cell ra i)))
; it should be easier :-/
             (match-lambda*
               ((o) (ra-set! ra o))
               ((i0 o) (ra-set! ra o i0))
               ((i0 i1 o) (ra-set! ra o i0 i1))
               ((i0 i1 i2 o) (ra-set! ra o i0 i1 i2))
               ((i0 i1 i2 i3 o) (ra-set! ra o i0 i1 i2 i3))
               ((i ... o) (apply ra-set! ra o i)))
             data zero dims type vlen vref vset!)))
    ra))

;; data:    a container (function) addressable by a single integer
;; address: into data.
;; zero:    address that corresponds to all the ra indices = 0.
;; %:       regular macro.
;; %%:      skip ra? check.

(define-syntax %%struct-ref (syntax-rules () ((_ a n) (struct-ref a n))))
(define-syntax %%struct-set! (syntax-rules () ((_ a n o) (struct-set! a n o))))

(define-inlinable (%%ra-root a) (%%struct-ref a 2))
(define-inlinable (%%ra-zero a) (%%struct-ref a 3))
(define-inlinable (%%ra-zero-set! a z) (%%struct-set! a 3 z)) ; set on iteration. FIXME immutable record?
(define-inlinable (%%ra-dims a) (%%struct-ref a 4))
(define-inlinable (%%ra-type a) (%%struct-ref a 5))
(define-inlinable (%%ra-vlen a) (%%struct-ref a 6))
(define-inlinable (%%ra-vref a) (%%struct-ref a 7))
(define-inlinable (%%ra-vset! a) (%%struct-ref a 8))

(define-syntax %rastruct-ref (syntax-rules () ((_ a n) (begin (check-ra a) (struct-ref a n)))))
(define-syntax %rastruct-set! (syntax-rules () ((_ a n o) (begin (check-ra a) (struct-set! a n o)))))

(define-inlinable (%ra-root a) (%rastruct-ref a 2))
(define-inlinable (%ra-zero a) (%rastruct-ref a 3))
(define-inlinable (%ra-zero-set! a z) (%rastruct-set! a 3 z))
(define-inlinable (%ra-dims a) (%rastruct-ref a 4))
(define-inlinable (%ra-type a) (%rastruct-ref a 5))
(define-inlinable (%ra-vlen a) (%rastruct-ref a 6))
(define-inlinable (%ra-vref a) (%rastruct-ref a 7))
(define-inlinable (%ra-vset! a) (%rastruct-ref a 8))

(define (ra-zero a)
  "
ra-zero ra -> i

Return the index I into the root vector of RA that corresponds to all array
indices being 0. Note that I may be outside the range of the root vector,
for example if RA is empty or its lower bounds are not 0.

See also: ra-offset
"
  (%ra-zero a))

(define (ra-root a)
  "
ra-root ra -> v

Return the root vector (or data vector) V of RA.
"
  (%ra-root a))

(define (ra-zero-set! a z) (%ra-zero-set! a z))
(define (ra-dims a) (%ra-dims a))
(define (ra-type a) (%ra-type a))
(define (ra-vlen a) (%ra-vlen a))
(define (ra-vref a) (%ra-vref a))
(define (ra-vset! a) (%ra-vset! a))

(define-inlinable (%%ra-step a k) (dim-step (vector-ref (%%ra-dims a) k)))

(define (pick-make-root type)
  (case type
    ((#t) make-vector)
    ((c64) make-c64vector)
    ((c32) make-c32vector)
    ((f64) make-f64vector)
    ((f32) make-f32vector)
    ((s64) make-s64vector)
    ((s32) make-s32vector)
    ((s16) make-s16vector)
    ((s8) make-s8vector)
    ((u64) make-u64vector)
    ((u32) make-u32vector)
    ((u16) make-u16vector)
    ((u8) make-u8vector)
    ((a) make-string)
    ((b) make-bitvector)
; TODO extend this idea to drag-along
    ((d) (throw 'no-dim-make))
    (else (throw 'bad-ra-root-type type))))

(define (pick-root-functions v)
  (cond ((vector? v)    (values  #t    vector-length     vector-ref     vector-set!   ))
        ((c64vector? v) (values  'c64  c64vector-length  c64vector-ref  c64vector-set!))
        ((c32vector? v) (values  'c32  c32vector-length  c32vector-ref  c32vector-set!))
        ((f64vector? v) (values  'f64  f64vector-length  f64vector-ref  f64vector-set!))
        ((f32vector? v) (values  'f32  f32vector-length  f32vector-ref  f32vector-set!))
        ((s64vector? v) (values  's64  s64vector-length  s64vector-ref  s64vector-set!))
        ((s32vector? v) (values  's32  s32vector-length  s32vector-ref  s32vector-set!))
        ((s16vector? v) (values  's16  s16vector-length  s16vector-ref  s16vector-set!))
        ((s8vector? v)  (values  's8   s8vector-length   s8vector-ref   s8vector-set! ))
        ((u64vector? v) (values  'u64  u64vector-length  u64vector-ref  u64vector-set!))
        ((u32vector? v) (values  'u32  u32vector-length  u32vector-ref  u32vector-set!))
        ((u16vector? v) (values  'u16  u16vector-length  u16vector-ref  u16vector-set!))
        ((u8vector? v)  (values  'u8   u8vector-length   u8vector-ref   u8vector-set! ))
        ((string? v)    (values  'a    string-length     string-ref     string-set!   ))
        ((bitvector? v) (values  'b    bitvector-length  bitvector-ref  bitvector-set!))
; TODO extend this idea to drag-along
        ((dim? v)       (values  'd    dim-len           dim-ref        (cut throw 'no-dim-set! <...>)))
        (else (throw 'bad-ra-root-type v))))

; low level, for conversions
(define (make-ra-raw data zero dims)
  "
make-ra-raw data zero dims -> ra

Make new ra RA from root vector DATA, zero index ZERO and dim-vector DIMS.

See also: ra-data ra-zero ra-dims
"
  (unless (vector? dims) (throw 'bad-dims dims))
  (vector-for-each (lambda (dim) (unless (dim? dim) (throw 'bad-dim dim))) dims)
; after check
  (receive (type vlen vref vset!) (pick-root-functions data)
    (make-ra* data zero dims type vlen vref vset!)))


; ----------------
; compute addresses
; ----------------

(define-syntax %ra-pos
  (syntax-rules ()
    ((_ j pos dims)
     pos)
    ((_ j pos dims i0 i ...)
     (let ((dim (vector-ref dims j)))
       (%ra-pos (+ j 1) (+ pos (* (dim-check dim i0) (dim-step dim))) dims i ...)))))

(define ra-pos
  (case-lambda
   ((zero dims) (%ra-pos 0 zero dims))
   ((zero dims i0) (%ra-pos 0 zero dims i0))
   ((zero dims i0 i1) (%ra-pos 0 zero dims i0 i1))
   ((zero dims i0 i1 i2) (%ra-pos 0 zero dims i0 i1 i2))
   ((zero dims i0 i1 i2 i3) (%ra-pos 0 zero dims i0 i1 i2 i3))
   ((zero dims . i_)
    (let loop ((j 0) (pos zero) (i i_))
      (if (null? i)
        pos
        (if (>= j (vector-length dims))
          (throw 'too-many-indices i_)
          (let ((dim (vector-ref dims j)))
            (loop (+ j 1) (+ pos (* (dim-check dim (car i)) (dim-step dim))) (cdr i)))))))))

; lowest position on data.
(define (ra-pos-lo ra)
  (check-ra ra)
  (let ((dims (%%ra-dims ra)))
    (let loop ((j (- (vector-length dims) 1)) (pos (%%ra-zero ra)))
      (if (< j 0)
        pos
        (let* ((dim (vector-ref dims j))
               (step (dim-step dim)))
          (loop (- j 1) (+ pos (* step (if (positive? step) (dim-lo dim) (dim-hi dim))))))))))

; highest position on data.
(define (ra-pos-hi ra)
  (check-ra ra)
  (let ((dims (%%ra-dims ra)))
    (let loop ((j (- (vector-length dims) 1)) (pos (%%ra-zero ra)))
      (if (< j 0)
        pos
        (let* ((dim (vector-ref dims j))
               (step (dim-step dim)))
          (loop (- j 1) (+ pos (* step (if (positive? step) (dim-hi dim) (dim-lo dim))))))))))

(define ra-offset
  (case-lambda
   "
ra-offset ra -> i

Return the root vector index I that corresponds to all array indices being equal
to the lower bound of RA in each dimension.

See also: ra-zero
"
   ((ra)
    (let ((ra (check-ra ra)))
      (ra-offset (%%ra-zero ra) (%%ra-dims ra))))
; internally - useful for some types of loops, or to transition from Guile C arrays.
   ((zero dims)
    (ra-offset zero dims (vector-length dims)))
   ((zero dims k)
; min - enable prefix match, ignoring dead axes [(vector-length dims) ... (- k 1)]
    (let loop ((j (min k (vector-length dims))) (pos zero))
      (if (<= j 0)
        pos
        (let* ((j (- j 1))
               (dim (vector-ref dims j)))
          (loop j (+ pos (* (dim-lo dim) (dim-step dim))))))))))


; ----------------
; ref, set!, prefix slices
; ----------------

(define-inlinable (%%ra-rank a) (vector-length (%%ra-dims a)))
(define-inlinable (%ra-rank a) (vector-length (%ra-dims a)))
(define (ra-rank a) (%ra-rank a))

(define-syntax %length
  (syntax-rules ()
    ((_) 0)
    ((_ i0 i ...) (+ 1 (%length i ...)))))

(define ra-ref
  (let-syntax
      ((%args
        (syntax-rules  ()
          ((_ ra i ...)
           (begin
             (unless (= (%ra-rank ra) (%length i ...))
               (throw 'bad-number-of-indices (%ra-rank ra) (%length i ...)))
             ((%%ra-vref ra) (%%ra-root ra) (%ra-pos 0 (%%ra-zero ra) (%%ra-dims ra) i ...)))))))
    (case-lambda
      ((ra) (%args ra))
      ((ra i0) (%args ra i0))
      ((ra i0 i1) (%args ra i0 i1))
      ((ra i0 i1 i2) (%args ra i0 i1 i2))
      ((ra i0 i1 i2 i3) (%args ra i0 i1 i2 i3))
      ((ra . i)
       (unless (= (%ra-rank ra) (length i))
         (throw 'bad-number-of-indices (%ra-rank ra) (length i)))
       ((%%ra-vref ra) (%%ra-root ra) (apply ra-pos (%%ra-zero ra) (%%ra-dims ra) i))))))

(define ra-set!
  (let-syntax
      ((%args
        (syntax-rules ()
          ((_ ra o i ...)
           (begin
             (unless (= (%ra-rank ra) (%length i ...))
               (throw 'bad-number-of-indices (%ra-rank ra) (%length i ...)))
             ((%%ra-vset! ra) (%%ra-root ra) (%ra-pos 0 (%%ra-zero ra) (%%ra-dims ra) i ...) o)
             ra)))))
    (case-lambda
      ((ra o) (%args ra o))
      ((ra o i0) (%args ra o i0))
      ((ra o i0 i1) (%args ra o i0 i1))
      ((ra o i0 i1 i2) (%args ra o i0 i1 i2))
      ((ra o i0 i1 i2 i3) (%args ra o i0 i1 i2 i3))
      ((ra o . i)
       (unless (= (%ra-rank ra) (length i))
         (throw 'bad-number-of-indices (%ra-rank ra) (length i)))
       ((%%ra-vset! ra) (%%ra-root ra) (apply ra-pos (%%ra-zero ra) (%%ra-dims ra) i) o)
       ra))))

(define (ra-slice ra . i)
  (check-ra ra)
  (make-ra-raw (%%ra-root ra)
               (apply ra-pos (%%ra-zero ra) (%%ra-dims ra) i)
               (vector-drop (%%ra-dims ra) (length i))))

; Unhappy about writing these things twice.
(define ra-cell
  (letrec-syntax
      ((%args
        (syntax-rules ()
          ((_ ra i ...) (%ra-pos 0 (%%ra-zero ra) (%%ra-dims ra) i ...))))
       (%cell
        (syntax-rules ()
          ((_ ra i ...)
           (let ((pos (%args ra i ...))
                 (leni (%length i ...)))
             (check-ra ra)
             (if (= (%%ra-rank ra) leni)
               ((%%ra-vref ra) (%%ra-root ra) pos)
               (make-ra-raw (%%ra-root ra) pos (vector-drop (%%ra-dims ra) leni))))))))
    (case-lambda
     ((ra) (%cell ra))
     ((ra i0) (%cell ra i0))
     ((ra i0 i1) (%cell ra i0 i1))
     ((ra i0 i1 i2) (%cell ra i0 i1 i2))
     ((ra i0 i1 i2 i3) (%cell ra i0 i1 i2 i3))
     ((ra . i)
      (check-ra ra)
      (let ((pos (apply ra-pos (%%ra-zero ra) (%%ra-dims ra) i))
            (leni (length i)))
        (if (= (%%ra-rank ra) leni)
          ((%%ra-vref ra) (%%ra-root ra) pos)
          (make-ra-raw (%%ra-root ra) pos (vector-drop (%%ra-dims ra) leni))))))))


; ----------------
; derived functions
; ----------------

; FIXME avoid list->vector etc.

(define (c-dims . d)
  "
c-dims d ...

Compute dim-vector for C-order (row-major) array of sizes D ...

The first size may given as #f, which indicates an infinite dimension.

See also: make-ra-root make-ra-new
"
  (list->vector
   (let loop ((d d))
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

(define (make-ra-root data dims)
  (make-ra-raw data (- (ra-offset 0 dims)) dims))

(define (make-ra-new type value dims)
  "
make-ra-new type value dims -> RA

Make new ra RA of TYPE from dim-vector DIMS, and fill it with VALUE. VALUE may
be *unspecified*.

See also: make-dim ra-dims make-ra-root c-dims
"
  (let ((size (vector-fold
               (lambda (a c)
                 (* c (let ((len (dim-len a)))
                        (or len (if (zero? (dim-step a)) 1 (throw 'cannot-make-new-ra-with-dims dims))))))
               1 dims))
        (make (pick-make-root type)))
    (make-ra-raw (if (unspecified? value) (make size) (make size value))
                 (- (ra-offset 0 dims))
                 dims)))
