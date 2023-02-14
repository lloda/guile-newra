; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2016-2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Basic definitions for arrays.
;;; Code:

(define-module (newra base)
  #:export (ra?
            ra-root ra-zero ra-zero-set! ra-dims ra-type ra-vlen ra-vref ra-vset!
            ra-check
            ra-rank ra-type make-ra-new make-ra-root
            make-aseq aseq? aseq-org aseq-inc aseq-ref
            make-dim dim? dim-len dim-lo dim-hi dim-step c-dims
            ra-pos ra-offset
            ra-slice ra-cell ra-ref ra-set!
; for internal (newra) use, don't re-export from (newra)
            define-inlinable-case
            <aseq> <dim> make-dim* dim-check
            <ra-vtable> pick-functions pick-make
            %%ra-root %%ra-zero %%ra-type %%ra-rank
            %%ra-zero-set! %%ra-dims %%ra-vlen %%ra-vref %%ra-vset! %%ra-step
            ra-shape ra-dimensions ra-len ra-lo ra-size))

(import (srfi 26) (srfi 2) (srfi 71) (srfi srfi-4 gnu) (srfi srfi-9 gnu)
        (only (srfi 1) fold every) (ice-9 match) (ice-9 control)
        (rnrs bytevectors) (only (rnrs base) vector-for-each)
        (newra vector))


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
;; i, j:        indices in a dim, from lo to hi
;; k:           an index in a dim vector, from 0 to rank-1
;; slice:       an ra, as a piece of another ra
;; cell:        (also prefix-cell) slice obtained by fixing the first k indices into an ra.
;; item:        slice obtained by fixing the first index into an ra; a (rank - 1)-cell.


; ----------------
; misc - FIXME remove if unused
; ----------------

; cf https://www.scheme.com/tspl4/syntax.html - define-integrable
; cf guile/module/ice-9/boot.scm - define-inlinable

(define-syntax define-inlinable-case
  (lambda (x)
    (define prefix (string->symbol "% "))
    (define (make-procedure-name name)
      (datum->syntax name (symbol-append prefix (syntax->datum name) '-procedure)))
    (syntax-case x (case-lambda)
      ((_ name (case-lambda DOC (formals form1 form2 ...) ...))
       (and (identifier? #'name)
            (string? (syntax->datum #'DOC)))
       (with-syntax ((xname (make-procedure-name #'name)))
         #`(begin
             (define xname
               (syntax-parameterize ((name (identifier-syntax xname)))
                 (case-lambda DOC (formals form1 form2 ...) ...)))
             (define-syntax-parameter name
               (lambda (x)
                 (syntax-case x ()
                   (_ (identifier? x) #'xname)
                   ((_ arg (... ...))
                    #'((syntax-parameterize ((name (identifier-syntax xname)))
                         (case-lambda DOC (formals form1 form2 ...) ...))
                       arg (... ...)))))))))
      ((_ name (case-lambda (formals form1 form2 ...) ...))
       #'(define-inlinable-case name (case-lambda "" (formals form1 form2 ...) ...))))))


; ----------------
; arithmetic sequence for ra roots
; ----------------

(define-immutable-record-type <aseq>
  (make-aseq* org inc) aseq?
  (org aseq-org)
  (inc aseq-inc))

(define-inlinable-case make-aseq
  (case-lambda
   (() (make-aseq* 0 1))
   ((org)
    (unless (real? org) (throw 'bad-aseq-org org))
    (make-aseq* org 1))
   ((org inc)
    (unless (real? org) (throw 'bad-aseq-org org))
    (unless (real? inc) (throw 'bad-aseq-inc inc))
    (make-aseq* org inc))))

(define-inlinable (aseq-ref aseq i)
  (+ (aseq-org aseq) (* i (aseq-inc aseq))))


; ----------------
; dimension of array axes
; ----------------

(define-immutable-record-type <dim>
  (make-dim* len lo step) dim?
  (len dim-len)
  (lo dim-lo)
  (step dim-step))

(define-inlinable-case make-dim
  (case-lambda
   "
make-dim len
make-dim len lo
make-dim len lo step

Create an ra axis descriptor with the given parameters.

See also: dim-len dim-lo dim-step c-dims
"
   ((len) (make-dim len 0 1))
   ((len lo) (make-dim len lo 1))
   ((len lo step)
    (when (and len (or (not (integer? len)) (negative? len))) (throw 'bad-dim-len len))
; lo #f requires len #f. FIXME doc when that can happen.
    (when (and (not lo) len) (throw 'bad-dim-lo-len lo len))
    (make-dim* len lo step))))

(define-inlinable (dim-end dim)
  (+ (dim-lo dim) (dim-len dim)))

(define-inlinable (dim-hi dim)
  (let ((len (dim-len dim)))
    (and len (+ (dim-lo dim) (dim-len dim) -1))))

(define-inlinable (dim-check dim i)
  (if (integer? i)
    (if (let ((lo (dim-lo dim)))
          (and
           (or (not lo) (>= i lo))
           (let ((len (dim-len dim)))
             (or (not len) (< i (+ len lo)))))) ; len implies lo
      i
      (throw 'dim-check-out-of-range dim i))
; FIXME catch & forward to ra-from in make-ra* applications
    (throw 'dim-check-index-not-integer dim i)))


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

(define-inlinable (ra-check o)
  (if (ra? o) o (throw 'not-ra? o)))

;; data:    a container (function) addressable by a single integer
;; address: into data.
;; zero:    address that corresponds to all the ra indices = 0.
;; %:       regular macro.
;; %%:      skip ra? check.

(define-inlinable (%%ra-root a) (struct-ref a 2))
(define-inlinable (%%ra-zero a) (struct-ref a 3))
(define-inlinable (%%ra-zero-set! a z) (struct-set! a 3 z)) ; set on iteration. Everything else immutable.
(define-inlinable (%%ra-dims a) (struct-ref a 4))
(define-inlinable (%%ra-type a) (struct-ref a 5))
(define-inlinable (%%ra-vlen a) (struct-ref a 6))
(define-inlinable (%%ra-vref a) (struct-ref a 7))
(define-inlinable (%%ra-vset! a) (struct-ref a 8))

(define-syntax-rule (%rastruct-ref a n) (struct-ref (ra-check a) n))
(define-syntax-rule (%rastruct-set! a n o) (struct-set! (ra-check a) n o))

(define-inlinable (ra-root a)
  "
Return the root vector of array @var{a}.
"
  (%rastruct-ref a 2))

(define-inlinable (ra-zero a)
  "
Return the index @var{i} into the root vector of @var{ra} that corresponds to
all array indices being 0. Note that @var{i} may be outside the range of the
root vector, for example if @var{a} is empty or its lower bounds are positive.

See also: @code{ra-offset}
"
  (%rastruct-ref a 3))

(define-inlinable (ra-zero-set! a z) (%rastruct-set! a 3 z))
(define-inlinable (ra-dims a) (%rastruct-ref a 4))
(define-inlinable (ra-type a) (%rastruct-ref a 5))
(define-inlinable (ra-vlen a) (%rastruct-ref a 6))
(define-inlinable (ra-vref a) (%rastruct-ref a 7))
(define-inlinable (ra-vset! a) (%rastruct-ref a 8))

(define-inlinable (%%ra-step a k) (dim-step (vector-ref (%%ra-dims a) k)))

(define (pick-make type)
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
    ((vu8) make-u8vector)
    ((a) make-string)
    ((b) make-bitvector)
    ((d) (throw 'no-dim-make))
    (else (throw 'bad-ra-root-type type))))

(define (pick-functions v)
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
        ((bytevector? v)  (values  'u8   u8vector-length   u8vector-ref   u8vector-set!))
        ((string? v)    (values  'a    string-length     string-ref     string-set!   ))
        ((bitvector? v) (values  'b    bitvector-length  bitvector-ref  bitvector-set!))
; TODO extend this to drag-along.
        ((aseq? v)      (values  'd    (const #f)        aseq-ref       (cut throw 'no-aseq-set! <...>)))
        (else (throw 'bad-ra-root v))))


; ----------------
; compute addresses
; ----------------

(define-syntax %ra-pos
  (syntax-rules ()
    ((_ j pos dims)
     pos)
    ((_ j pos dims i0 i ...)
     (let ((dim (vector-ref dims j)))
       (dim-check dim i0)
       (%ra-pos (+ j 1) (+ pos (* i0 (dim-step dim))) dims i ...)))))

(define-inlinable-case ra-pos
  (case-lambda
   ((zero dims) (%ra-pos 0 zero dims))
   ((zero dims i0) (%ra-pos 0 zero dims i0))
   ((zero dims i0 i1) (%ra-pos 0 zero dims i0 i1))
   ((zero dims i0 i1 i2) (%ra-pos 0 zero dims i0 i1 i2))
   ((zero dims i0 i1 i2 i3) (%ra-pos 0 zero dims i0 i1 i2 i3))
   ((zero dims i0 i1 i2 i3 i4) (%ra-pos 0 zero dims i0 i1 i2 i3 i4))
   ((zero dims . i_)
    (let loop ((j 0) (pos zero) (i i_))
      (if (null? i)
        pos
        (if (>= j (vector-length dims))
          (throw 'too-many-indices i_)
          (let ((dim (vector-ref dims j)))
            (loop (+ j 1) (+ pos (* (dim-check dim (car i)) (dim-step dim))) (cdr i)))))))))

(define-inlinable-case ra-offset
  (case-lambda
   "
Return the root vector index @var{i} that corresponds to all ra indices being
equal to the lower bound of @var{ra} in axes [@var{org} ... @var{org}+@var{k}).

See also: @code{ra-zero}
"
   ((ra)
    (let ((ra (ra-check ra)))
      (ra-offset (%%ra-zero ra) (%%ra-dims ra))))
; internally - useful for some types of loops, or to transition from Guile C arrays.
   ((zero dims)
    (ra-offset zero dims (vector-length dims) 0))
   ((zero dims k)
    (ra-offset zero dims k 0))
   ((zero dims k org)
; min - enable prefix match, ignoring dead axes [(vector-length dims) ... (- k 1)]
    (let loop ((k (min (+ k org) (vector-length dims))) (pos zero))
      (if (<= k org)
        pos
        (let* ((k (- k 1))
               (dim (vector-ref dims k)))
          (loop k (+ pos (* (or (dim-lo dim) 0) (dim-step dim))))))))))


; ----------------
; ref, set!, prefix slices
; ----------------

(define-inlinable (%%ra-rank a) (vector-length (%%ra-dims a)))
(define-inlinable (ra-rank a) (vector-length (ra-dims a)))

(define-syntax %length
  (syntax-rules ()
    ((_) 0)
    ((_ i0 i ...) (+ 1 (%length i ...)))))

; FIXME would like to use let-syntax for these macros that are only used in one place.

(define-syntax %ra-ref
  (syntax-rules  ()
    ((_ ra i ...)
     (begin
       (unless (= (ra-rank ra) (%length i ...))
         (throw 'bad-number-of-indices (ra-rank ra) (%length i ...)))
       ((%%ra-vref ra) (%%ra-root ra) (%ra-pos 0 (%%ra-zero ra) (%%ra-dims ra) i ...))))))

(define-inlinable-case ra-ref
  (case-lambda
   "
Return the element of ra @var{a} determined by indices @var{i}. The number of
indices must be equal to the rank of @var{a}.

For example:

@lisp
(ra-ref (ra-i 2 3) 1 1)
@result{} 5
@end lisp

See also: @code{ra-cell} @code{ra-slice} @code{ra-from}
"
   ((ra) (%ra-ref ra))
   ((ra i0) (%ra-ref ra i0))
   ((ra i0 i1) (%ra-ref ra i0 i1))
   ((ra i0 i1 i2) (%ra-ref ra i0 i1 i2))
   ((ra i0 i1 i2 i3) (%ra-ref ra i0 i1 i2 i3))
   ((ra i0 i1 i2 i3 i4) (%ra-ref ra i0 i1 i2 i3 i4))
   ((ra . i)
    (unless (= (ra-rank ra) (length i))
      (throw 'bad-number-of-indices (ra-rank ra) (length i)))
    ((%%ra-vref ra) (%%ra-root ra) (apply ra-pos (%%ra-zero ra) (%%ra-dims ra) i)))))

(define-syntax %ra-set!
  (syntax-rules ()
    ((_ ra o i ...)
     (begin
       (unless (= (ra-rank ra) (%length i ...))
         (throw 'bad-number-of-indices (ra-rank ra) (%length i ...)))
       ((%%ra-vset! ra) (%%ra-root ra) (%ra-pos 0 (%%ra-zero ra) (%%ra-dims ra) i ...) o)
       ra))))

(define-inlinable-case ra-set!
  (case-lambda
   ((ra o) (%ra-set! ra o))
   ((ra o i0) (%ra-set! ra o i0))
   ((ra o i0 i1) (%ra-set! ra o i0 i1))
   ((ra o i0 i1 i2) (%ra-set! ra o i0 i1 i2))
   ((ra o i0 i1 i2 i3) (%ra-set! ra o i0 i1 i2 i3))
   ((ra o i0 i1 i2 i3 i4) (%ra-set! ra o i0 i1 i2 i3 i4))
   ((ra o . i)
    (unless (= (ra-rank ra) (length i))
      (throw 'bad-number-of-indices (ra-rank ra) (length i)))
    ((%%ra-vset! ra) (%%ra-root ra) (apply ra-pos (%%ra-zero ra) (%%ra-dims ra) i) o)
    ra)))

(define (ra-slice ra . i)
   "
Return the prefix cell of ra @var{a} determined by indices @var{i}. The number
of indices must be no larger than the rank of @var{a}.

This function always returns an array, even if the number of indices is equal to
the rank of @var{a}.

For example:

@lisp
(ra-slice (ra-i 2 3))
@result{} #%2((0 1 2) (4 5 6))
@end lisp

@lisp
(ra-slice (ra-i 2 3) 1)
@result{} #%1(4 5 6)
@end lisp

@lisp
(ra-slice (ra-i 2 3) 1 1)
@result{} #%0(5)
@end lisp

@code{ra-slice} can be used to copy an array descriptor; the return value
contains a fresh copy of the dim vector of @var{ra}.

See also: @code{ra-ref} @code{ra-cell} @code{ra-from}
"
  (let ((ra (ra-check ra)))
    (make-ra-root (%%ra-root ra)
                  (vector-drop (%%ra-dims ra) (length i))
                  (apply ra-pos (%%ra-zero ra) (%%ra-dims ra) i))))

; Unhappy about writing these things twice.
(define-syntax %ra-cell
  (syntax-rules ()
    ((_ ra i ...)
     (let ((ra (ra-check ra)))
       (let ((pos (%ra-pos 0 (%%ra-zero ra) (%%ra-dims ra) i ...))
             (leni (%length i ...)))
         (if (= (%%ra-rank ra) leni)
           ((%%ra-vref ra) (%%ra-root ra) pos)
           (make-ra-root (%%ra-root ra) (vector-drop (%%ra-dims ra) leni) pos)))))))

(define-inlinable-case ra-cell
  (case-lambda
   "
ra-cell a i ...

Return the prefix cell of ra @var{a} determined by indices @var{i}. The number
of indices must be no larger than the rank of @var{a}. If the number of indices
is equal to the rank of @var{a}, then return the corresponding element (same as
@code{ra-ref}) and not a rank-0 cell.

For example:

@lisp
(ra-cell (ra-i 2 3))
@result{} #%2((0 1 2) (4 5 6))
@end lisp

@lisp
(ra-cell (ra-i 2 3) 1)
@result{} #%1(4 5 6)
@end lisp

@lisp
(ra-cell (ra-i 2 3) 1 1)
@result{} 5
@end lisp

See also: @code{ra-ref} @code{ra-slice} @code{ra-from}
"
   ((ra) (%ra-cell ra))
   ((ra i0) (%ra-cell ra i0))
   ((ra i0 i1) (%ra-cell ra i0 i1))
   ((ra i0 i1 i2) (%ra-cell ra i0 i1 i2))
   ((ra i0 i1 i2 i3) (%ra-cell ra i0 i1 i2 i3))
   ((ra i0 i1 i2 i3 i4) (%ra-cell ra i0 i1 i2 i3 i4))
   ((ra . i)
    (let ((ra (ra-check ra)))
      (let ((pos (apply ra-pos (%%ra-zero ra) (%%ra-dims ra) i))
            (leni (length i)))
        (if (= (%%ra-rank ra) leni)
          ((%%ra-vref ra) (%%ra-root ra) pos)
          (make-ra-root (%%ra-root ra) (vector-drop (%%ra-dims ra) leni) pos)))))))

; these depend on accessor/setter.

(define (make-ra* data zero dims type vlen vref vset!)
  (letrec ((ra
            (make-struct/simple
             <ra-vtable>
             (case-lambda
               (() (ra-cell ra))
               ((i0) (ra-cell ra i0))
               ((i0 i1) (ra-cell ra i0 i1))
               ((i0 i1 i2) (ra-cell ra i0 i1 i2))
               ((i0 i1 i2 i3) (ra-cell ra i0 i1 i2 i3))
               ((i0 i1 i2 i3 i4) (ra-cell ra i0 i1 i2 i3 i4))
               (i (apply ra-cell ra i)))
; it should be easier :-/
             (match-lambda*
               ((o) (ra-set! ra o))
               ((i0 o) (ra-set! ra o i0))
               ((i0 i1 o) (ra-set! ra o i0 i1))
               ((i0 i1 i2 o) (ra-set! ra o i0 i1 i2))
               ((i0 i1 i2 i3 o) (ra-set! ra o i0 i1 i2 i3))
               ((i0 i1 i2 i3 i4 o) (ra-set! ra o i0 i1 i2 i3 i4))
               ((i ... o) (apply ra-set! ra o i)))
             data zero dims type vlen vref vset!)))
    ra))

; low level, for conversions
(define make-ra-root
  (case-lambda
   "
Make new array from root vector @var{root}, zero index @var{zero} and dim-vector
@var{dims}.

If @var{zero} is absent, it is computed so that the first element of the result
is the first element of the root, that is, @code{(ra-offset ra)} is 0.

If @var{dims} is absent, make a rank-1 array with the full length of @var{root}.

See also: @code{ra-root} @code{ra-zero} @code{ra-dims}
"
   ((root dims zero)
    (when dims
      (unless (vector? dims) (throw 'bad-dims dims))
      (vector-for-each (lambda (dim) (unless (dim? dim) (throw 'bad-dim dim))) dims))
; after check
    (let ((type vlen vref vset! (pick-functions root)))
      (make-ra* root zero
                (or dims (vector (make-dim (vlen root))))
                type vlen vref vset!)))
   ((root dims)
    (make-ra-root root dims (- (ra-offset 0 dims))))
   ((root)
    (make-ra-root root #f 0))))


; ----------------
; derived functions
; ----------------

; FIXME avoid list->vector etc.

(define (c-dims . d)
  "
Compute dim-vector for C-order (row-major) array of bounds @var{d} ...

Each of the @var{d} ... may be @var{len}, or a bounds pair (@var{lo}
@var{hi}). If @var{len} or @var{hi} is @code{#f}, this creates a dead axis.

The first non-@code{#f} @var{hi} or @var{len} may be @code{#t}; this creates an
unbounded axis.

See also: @code{make-ra-root} @code{make-ra-new}
"
  (let ((d (list->vector d)))
    (let loop ((i (- (vector-length d) 1)) (step 1))
      (if (negative? i)
        d
        (match (vector-ref d i)
          ((lo #f)
           (vector-set! d i (make-dim #f lo 0))
           (loop (- i 1) step))
          ((lo #t)
           (vector-set! d i (make-dim #f lo step))
           (loop (- i 1) #f))
          ((lo hi)
           (let ((len (- hi lo -1)))
             (vector-set! d i (make-dim len lo step))
             (loop (- i 1) (* len step))))
          (#t
           (vector-set! d i (make-dim #f 0 step))
           (loop (- i 1) #f))
          (#f
           (vector-set! d i (make-dim #f 0 0))
           (loop (- i 1) step))
          (len
           (vector-set! d i (make-dim len 0 step))
           (loop (- i 1) (* len step))))))))

(define (make-ra-new type value dims)
  "
Make new array of @var{type} from dim-vector @var{dims}, and fill it with
@var{value}. @var{value} may be @code{*unspecified*}.

See also: @code{make-dim} @code{ra-dims} @code{make-ra-root} @code{c-dims}
"
  (let ((size (vector-fold
               (lambda (a c)
                 (* c (let ((len (dim-len a)))
                        (or len (if (zero? (dim-step a)) 1 (throw 'cannot-make-new-ra-with-dims dims))))))
               1 dims))
        (make (pick-make type)))
    (make-ra-root (if (unspecified? value) (make size) (make size value))
                  dims
                  (- (ra-offset 0 dims)))))


; ----------------
; misc functions for Guile compatibility
; ----------------

(define (ra-shape ra)
  "
Return a list with the lower and upper bounds of each dimension of @var{ra}.

@lisp
(ra-shape (make-ra 'foo '(-1 3) 5)) ==> ((-1 3) (0 4))
@end lisp

See also: @code{ra-rank} @code{ra-dimensions} @code{ra-len}
"
  (map (lambda (dim) (list (dim-lo dim) (dim-hi dim))) (vector->list (ra-dims ra))))

(define (ra-dimensions ra)
  "
Like ra-shape, but if the lower bound for a given dimension is zero, return
the size of that dimension instead of a lower bound - upper bound pair.

@lisp
(ra-shape (make-ra 'foo '(-1 3) 5)) ==> ((-1 3) (0 4))
(ra-dimensions (make-ra 'foo '(-1 3) 5)) ==> ((-1 3) 5)
@end lisp

See also: @code{ra-rank} @code{ra-shape} @code{ra-len}
"
  (map (lambda (dim)
         (let ((lo (dim-lo dim)))
           (if (or (not lo) (zero? lo))
             (dim-len dim)
             (list lo (dim-hi dim)))))
    (vector->list (ra-dims ra))))

(define* (ra-len ra #:optional (k 0))
  "
Return the length of axis @var{k} of array @var{ra}. @var{k} defaults to 0. It
is an error if @var{ra} has zero rank.

See also: @code{ra-shape} @code{ra-dimensions} @code{ra-size} @code{ra-lo}
"
  (dim-len (vector-ref (ra-dims ra) k)))

(define* (ra-lo ra #:optional (k 0))
  "
Return the lower bound of axis @var{k} of array @var{ra}. @var{k} defaults to
0. It is an error if @var{ra} has zero rank.

See also: @code{ra-shape} @code{ra-dimensions} @code{ra-len}
"
  (dim-lo (vector-ref (ra-dims ra) k)))

(define* (ra-size ra #:optional (n (ra-rank ra)) (org 0))
  "
Return the product of the lengths of axes [@var{org} .. @var{org}+@var{n}) of
@var{ra}.

@var{n} defaults to the rank of @var{ra} and @var{org} defaults to 0, so by
default @code{(ra-size ra)} will return the number of elements of
@var{ra}. Arrays of rank 0 have size 1.

See also: @code{ra-shape} @code{ra-dimensions} @code{ra-len}
"
  (vector-fold* n org (lambda (d s) (* s (dim-len d))) 1 (ra-dims ra)))
