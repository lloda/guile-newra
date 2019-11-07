; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2018-2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Library for Newra - old arrays compatiblity, etc.
;;; Code:

(define-module (newra lib)
  #:export (ra-index-map!
            ra-length ra-size make-ra make-typed-ra make-ra-shared ra->list
            ra-dimensions ra-shape
            array->ra ra->array as-ra
            ra-i ra-iota
            ra-copy
            ra-reverse ra-transpose ra-order-c? ra-ravel ra-reshape ra-tile
            ra-fold ra-fold*

            vector-append))

(import (newra base) (newra map) (only (srfi :1) fold every any iota drop) (srfi :8) (srfi :26)
        (ice-9 control) (ice-9 match) (only (rnrs base) vector-map vector-for-each))

(define (vector-append . a)
  (let ((b (make-vector (fold (lambda (a c) (+ (vector-length a) c)) 0 a))))
    (let loopa ((a a) (lo 0))
      (if (null? a)
        b
        (let ((lena (vector-length (car a))))
          (do ((j 0 (+ j 1)))
              ((= j lena))
            (vector-set! b (+ lo j) (vector-ref (car a) j)))
          (loopa (cdr a) (+ lo lena)))))))


; ----------------
; fold - probably better ways to do this by fixing or extending (newra map)
; ----------------

(define-inlinable-case ra-fold
  (case-lambda
   "
ra-fold kons knil ra ...

Reduce ra RA by (... (KONS RA1 ... (KONS RA0 ... KNIL)) ...) where
(RA0 ...), (RA1 ...) is some sequence of the elements of RA ....

See also: ra-fold* ra-map! ra-for-each ra-slice-for-each
"
   ((op init)
    init)
   ((op init a0)
    (ra-for-each (lambda (x0) (set! init (op x0 init))) a0)
    init)
   ((op init a0 a1)
    (ra-for-each (lambda (x0 x1) (set! init (op x0 x1 init))) a0 a1)
    init)
   ((op init a0 a1 a2)
    (ra-for-each (lambda (x0 x1 x2) (set! init (op x0 x1 x2 init))) a0 a1 a2)
    init)
   ((op init . args)
    (apply ra-for-each (lambda x (set! init (apply op (append! x (list init))))) args)
    init)))

; really prefer this order, possibly get rid of (ra-fold).
(define-inlinable-case ra-fold*
  (case-lambda
   "
ra-fold* kons knil ra ...

Reduce ra RA by (... (KONS (KONS KNIL RA0 ...) RA1 ... ) ...) where
(RA0 ...), (RA1 ...) is some sequence of the elements of RA ....

See also: ra-fold ra-map! ra-for-each ra-slice-for-each
"
   ((op init)
    init)
   ((op init a0)
    (ra-for-each (lambda (x0) (set! init (op init x0))) a0)
    init)
   ((op init a0 a1)
    (ra-for-each (lambda (x0 x1) (set! init (op init x0 x1))) a0 a1)
    init)
   ((op init a0 a1 a2)
    (ra-for-each (lambda (x0 x1 x2) (set! init (op init x0 x1 x2))) a0 a1 a2)
    init)
   ((op init . args)
    (apply ra-for-each (lambda x (set! init (apply op init x))) args)
    init)))


; ----------------
; transition help
; ----------------

(define (array->ra a)
  (let ((dims (list->vector
               (map (lambda (b i) (make-dim (- (cadr b) (car b) -1) (car b) i))
                 (array-shape a)
                 (shared-array-increments a)))))
    (make-ra-raw (shared-array-root a)
                 (- (shared-array-offset a) (ra-offset 0 dims))
                 dims)))

(define (ra->array ra)
  (when (eq? 'd (ra-type ra))
    (throw 'nonconvertible-type (ra-type ra)))
  (apply make-shared-array (ra-root ra)
         (lambda i (list (apply ra-pos (ra-zero ra) (ra-dims ra) i)))
         (vector->list (vector-map (lambda (dim) (list (dim-lo dim) (dim-hi dim)))
                                   (ra-dims ra)))))


; ----------------
; misc functions for Guile compatibility
; ----------------

(define (ra-shape ra)
  "
ra-shape ra

Return a list with the lower and upper bounds of each dimension of RA.

(ra-shape (make-ra 'foo '(-1 3) 5)) ==> ((-1 3) (0 4))

See also: ra-rank ra-dimensions ra-length
"
  (map (lambda (dim) (list (dim-lo dim) (dim-hi dim))) (vector->list (ra-dims ra))))

(define (ra-dimensions ra)
  "
ra-dimensions ra

Like ra-shape, but if the lower bound for a given dimension is zero, return
the size of that dimension instead of a lower bound - upper bound pair.

(ra-shape (make-ra 'foo '(-1 3) 5)) ==> ((-1 3) (0 4))
(ra-dimensions (make-ra 'foo '(-1 3) 5)) ==> ((-1 3) 5)

See also: ra-rank ra-shape ra-length
"
  (map (lambda (dim)
         (let ((lo (dim-lo dim)))
           (if (or (not lo) (zero? lo))
             (dim-len dim)
             (list lo (dim-hi dim)))))
    (vector->list (ra-dims ra))))

(define* (ra-length ra #:optional (k 0))
  "
ra-length ra [dim 0]

Return the length of the dimension DIM of ra RA. It is an error if RA has zero
rank.

See also: ra-shape ra-dimensions ra-size
"
  (dim-len (vector-ref (ra-dims ra) k)))

(define* (ra-size ra #:optional (n (ra-rank ra)))
  "
ra-size ra
ra-size ra n

Return the number of elements of ra RA, that is, the product of all its
lengths. Ras of rank 0 have size 1. If N is given, return the product of the
first N lengths.

See also: ra-shape ra-dimensions ra-length
"
  (vector-fold* n (lambda (d s) (* s (dim-len d))) 1 (ra-dims ra)))

(define (make-typed-ra type value . d)
  "
make-typed-ra type value d ...

Return a new ra of type TYPE and dimensions D. The ra will be initialized with
VALUE. VALUE may be *unspecified*, in which case the ra may be left
uninitialized.

See also: make-ra
"
  (make-ra-new type value (apply c-dims d)))

(define (make-ra value . d)
  "
make-ra value d ...

Equivalent to (make-typed-ra #t value d ...).

See also: make-typed-ra
"
  (make-ra-new #t value (apply c-dims d)))

(define (make-ra-shared oldra mapfunc . d)
  (ra-check oldra)
; get lo & len, won't use step except if the result is empty.
  (let* ((dims (apply c-dims d))
         (newrank (vector-length dims)))
; if the result *is* empty then it has no valid indices, so we cannot call mapfunc.
    (let emptycheck ((k 0))
      (if (< k newrank)
        (if (positive? (dim-len (vector-ref dims k)))
          (emptycheck (+ 1 k))
          (make-ra-raw (%%ra-root oldra) (%%ra-zero oldra) dims))
        (let* ((los (vector->list (vector-map dim-lo dims)))
               (ref (apply ra-pos (%%ra-zero oldra) (%%ra-dims oldra) (apply mapfunc los)))
               (dims (vector-map
                      (lambda (dim step) (make-dim (dim-len dim) (dim-lo dim) step))
                      dims
                      (let ((steps (make-vector newrank 0)))
                        (let loop ((k 0))
                          (cond
                           ((= k newrank) steps)
                           (else
                            (vector-set!
                             steps k
                             (if (positive? (dim-len (vector-ref dims k)))
                               (let ((ii (list-copy los)))
                                 (list-set! ii k (+ 1 (list-ref los k)))
                                 (- (apply ra-pos (%%ra-zero oldra) (%%ra-dims oldra) (apply mapfunc ii)) ref))
                               0))
                            (loop (+ k 1)))))))))
          (make-ra-raw (%%ra-root oldra) (- ref (ra-offset 0 dims)) dims))))))

; FIXME Depends on traversal order of ra-for-each.

(define (ra->list ra)
  "
ra->list ra

Return a nested list of the elements of ra RA. For example, if RA is a 1-rank
ra, the list contains the elements of RA; if RA is a 2-rank ra, the list
contains a list for each of the rows of RA; and so on.

See also: as-ra
"
  (let ((ra (ra-check ra))
        (rank (%%ra-rank ra)))
    (cond
     ((zero? rank) (ra-ref ra))
     (else
      (let ((ra (apply ra-reverse ra (iota rank)))
            (dimk (vector-ref (%%ra-dims ra) (- rank 1))))
        (let loop-rank ((ra ra))
          (cond
           ((= 1 (%%ra-rank ra))
            (if (> (dim-len dimk) 20)
              (ra-fold cons '() ra)
              (let loop-dim ((l '()) (i (dim-lo dimk)))
                (if (> i (dim-hi dimk))
                  l
                  (loop-dim (cons (ra-ref ra i) l) (+ i 1))))))
           (else
            (let ((l '()))
              (ra-slice-for-each 1 (lambda (x) (set! l (cons (loop-rank x) l))) ra)
              l)))))))))

; Similar to (@ (newra newra) ra-for-each-slice-1) - since we cannot unroll. It
; might be cheaper to go Fortran order (building the index lists back to front);
; should try that. C order and set-cdr! is how oldra does it.
; This function is provided for compatibility with oldra; generally we shouldn't
; be building index lists.

(define (ra-index-map! ra op)
  "
ra-index-map! ra op -> ra

Apply OP to the indices of each element of RA in turn, storing the result in the
corresponding element.  The value returned and the order of application are
unspecified.

This function returns the modified RA.

For example:

guile> (define x (make-ra 0 2 3))
guile> (ra-index-map! x (lambda x x))
guile> x
#%2:2:3(((0 0) (0 1) (0 2)) ((1 0) (1 1) (1 2)))

See also: ra-iota ra-i
"
  (let* ((kk (ra-rank ra))
         (ii (make-list kk)))
    (receive (los lens) ((@@ (newra newra) ra-slice-for-each-check) kk ra)
      (if (= kk 0)
        (ra-set! ra (apply op ii))
        (let loop-rank ((k 0) (ra ra) (endi ii))
          (let* ((lo (vector-ref los k))
                 (end (+ lo (vector-ref lens k))))
            (if (= (+ 1 k) kk)
              (let loop-dim ((i lo))
                (if (= i end)
                  (set-car! endi lo)
                  (begin
                    (set-car! endi i)
                    (ra-set! ra (apply op ii) i)
                    (loop-dim (+ i 1)))))
              (let loop-dim ((i lo))
                (if (= i end)
                  (set-car! endi lo)
                  (begin
                    (set-car! endi i)
                    (loop-rank (+ k 1) (ra-slice ra i) (cdr endi))
                    (loop-dim (+ i 1))))))))))
    ra))


; ----------------
; functions not found in oldra, or significantly extended
; ----------------

; FIXME partial implementation.

(define* (as-ra ra #:key (type (ra-type ra)) (new? #f))
  (cond ((and (eq? (ra-type ra) type) (not new?)) ra)
        (else (ra-copy! (make-ra-new
                         type *unspecified*
                         (apply c-dims (map dim-len (vector->list (ra-dims ra)))))
                        ra))))

(define (ra-i . i)
  (make-ra-root (make-aseq) (apply c-dims i)))

(define* (ra-iota len #:optional (lo 0) (step 1))
  (make-ra-root (make-aseq lo step) (c-dims len)))

; oldra has array-copy in (ice-9 arrays). Something of the sort.
; FIXME handling of dead axes could be faster - maybe c-dims should be written differently.

(define ra-copy
  (case-lambda
   "
ra-copy ra -> rb
ra-copy type ra -> rb

Copy the contents of RA into a new ra RB of type TYPE and the same shape as
RA. TYPE defaults to (ra-type RA) if not given.

If RA has dead axes, those are preserved in RB.

See also: ra-copy! as-ra
"
   ((ra) (ra-copy (ra-type ra) ra))
   ((type ra)
    (let* ((shape (map (match-lambda
                         (($ <dim> len lo step)
                          (let ((lo (or lo 0)))
                            (list lo
                                  (if len
                                    (+ lo len -1)
                                    (if (zero? step)
                                      lo
                                      (throw 'cannot-copy-infinite-ra ra)))))))
                    (vector->list (ra-dims ra))))
; copy destination needs the singletons else the lens don't match.
; FIXME we could have dead axes match dead axes.
           (rb (ra-copy! (make-ra-new type *unspecified* (apply c-dims shape)) ra)))
; preserve dead axes in the result.
      (if (any not (ra-dimensions ra))
        (make-ra-raw (ra-root rb) (ra-zero rb)
                     (vector-map (lambda (a b) (if (dim-len a) b a)) (ra-dims ra) (ra-dims rb)))
        rb)))))

(define (ra-reverse ra . k)
  "
ra-reverse ra k ...

Reverse axes K ... of ra RA, 0 <= K < (ra-rank RA). The contents of RA are not
copied.

Example:

  (ra-reverse (list->ra 2 '((0 1 2) (3 4 5))) 0 1)
    -> #%2:2:3((2 1 0) (5 4 3)))

See also: ra-transpose make-ra-shared
"
  (let* ((ra (ra-check ra))
         (ndims (vector-copy (%%ra-dims ra))))
    (let loop ((k k) (zero (%%ra-zero ra)))
      (if (null? k)
        (make-ra-raw (%%ra-root ra) zero ndims)
        (match (vector-ref ndims (car k))
          (($ <dim> len lo step)
           (vector-set! ndims (car k) (make-dim len lo (- step)))
           (loop (cdr k) (+ zero (* step (+ (* 2 lo) len -1))))))))))

(define (ra-transpose ra . axes)
  "
ra-transpose ra axes ... -> rb

Transpose the axes of ra RA. AXES must be a list of integers, and it must not be
longer than the rank of RA. Each axis i = 0 ... (ra-rank ra)-1 is transposed to
axis k = (AXES i) of RB. Therefore the rank of RB is 1+max(k).

An axis k of RB may be referenced multiple times in AXES, by RA axes i ... . In
that case the size of k is the smallest of the sizes of i ..., and the step of k
is the sum of all the steps of i ... . The lower bounds of i ... must all be the
same.

Any axis of RB that is not referenced in AXES is a `dead' axis with undefined
dimension and step 0.

Axes of RA that aren't listed in AXES are transposed to the end of RB's axes.

See also: make-ra-root make-ra-new
"
  (let* ((ra (ra-check ra))
         (odims (%%ra-dims ra))
         (orank (vector-length odims))
         (maxaxes (fold max -1 axes))
         (ndims (make-vector (+ 1 maxaxes) #f)))
    (do ((i 0 (+ i 1))
         (axesr axes (cdr axesr)))
        ((null? axesr)
; if not for lo we could have just initialized dims with (make-dim #f #f 0).
         (do ((k 0 (+ k 1)))
             ((= k (vector-length ndims)))
           (if (not (vector-ref ndims k))
             (vector-set! ndims k (make-dim #f #f 0))))
         (make-ra-raw (%%ra-root ra) (%%ra-zero ra)
; append rest axes
                      (vector-append ndims (vector-drop odims i))))
      (unless (< i orank)
        (throw 'bad-number-of-axes axes 'should-be i))
      (let* ((k (car axesr))
             (odim (vector-ref odims i))
             (ndim (vector-ref ndims k)))
        (vector-set!
         ndims k
         (if ndim
           (if (= (dim-lo odim) (dim-lo ndim))
             (make-dim (let* ((nd (dim-len ndim)) (od (dim-len odim)))
                         (if nd (and od (min nd od)) od))
                       (dim-lo ndim)
                       (+ (dim-step odim) (dim-step ndim)))
             (throw 'bad-lo))
           odim))))))

(define* (ra-order-c? ra #:optional n)
  "
ra-order-c? ra
ra-order-c? ra n

Check whether the N-frame of RA is in C-order (aka row-major order).

An ra frame is in C-order if the step of each axis is equal to the product of the
length and the step of the following axis. Axes with size 1 are ignored. If any axis
has size 0, the frame is in C-order.

If N is not given, check whether the elements of RA are in packed C-order. This
means that 1) the full frame of RA is in C-order, and 2) the step on the last
axis (whose size is neither 1 or 0) is 1.

See also: ra-ravel ra-reshape ra-tile c-dims
"
  (let* ((ra (ra-check ra))
         (dims (%%ra-dims ra))
         (rank (vector-length dims))
         (nn (or n rank)))
    (unless (<= 0 nn rank)
      (throw 'bad-number-of-axes n rank))
; look for an axis with len > 1
    (let loop ((i 0) (step #f))
      (if (>= i nn)
; no more axes; check the last step if n was #f
        (or (not step) n (= step 1))
        (match (vector-ref dims i)
          (($ <dim> ilen ilo istep)
           (case ilen
             ((0) 0) ; empty array
             ((1) (loop (+ i 1) step)) ; skip singleton axis
             (else
              (and (or (not step) ; first non-singleton axis
                       (= step (* ilen istep)))
                   (loop (+ i 1) istep))))))))))

; cf https://code.jsoftware.com/wiki/Vocabulary/comma

(define* (ra-ravel ra #:optional (n (ra-rank ra)))
  "
ra-ravel ra -> rb
ra-ravel ra n -> rb

Return the row-major ravel of the N-frame of RA. N defaults to the rank of RA.

The result RB is a ra with rank (rank(RA) - N + 1) that may or may not share the
root of RA.

When RB does not share the root of RA, then it has the same type as RA unless
that type is 'd, in which case it has type #t.

See also: ra-reshape ra-transpose ra-from ra-order-c?
"
  (define (pure-ravel ra n)
    (let ((od (%%ra-dims ra))
          (rank (vector-length (%%ra-dims ra))))
      (make-ra-raw (%%ra-root ra)
; the ravel is based at 0, so we don't want (ra-zero ra).
                   (ra-offset (%%ra-zero ra) od n)
                   (vector-append
                    (vector (make-dim (ra-size ra n) 0
                                      (cond ((zero? rank) 1)
                                            ((positive? n) (dim-step (vector-ref od (- n 1))))
                                            (else 1))))
                    (vector-drop od n)))))
  (pure-ravel
   (if (ra-order-c? ra n)
     ra
     (ra-copy! (make-ra-new (match (%%ra-type ra) ('d #t) (x x))
                               *unspecified* (apply c-dims (ra-dimensions ra)))
               ra))
   n))

; cf https://www.jsoftware.com/papers/APLDictionary1.htm#rho
; ... ⍺⍴⍵ produces a result of shape ⍺ from the elements of ⍵ ...

; cf https://code.jsoftware.com/wiki/Vocabulary/dollar#dyadic
; x $ y
; ... the shape of the result is (x,}.$y); i.e. the result is an array of items of y whose frame is x.

; cf http://www2.phys.canterbury.ac.nz/dept/docs/manuals/Fortran-90/HTMLNotesnode102.html#ReshapeIntrinsic1
; RESHAPE(SOURCE,SHAPE[,PAD][,ORDER])

(define (ra-reshape ra . s)
  "
ra-reshape ra s ... -> rb

Reshape the first axis of ra RA into shape S ... The shape of RB will be S
concatenated with the rest of the shape of RA. The total size of the new axes
must fit in the first axis of RA.

Each element of S is either a list of two integers (LO HI) or an integer LEN.

The result always shares the root of RA.

See also: ra-ravel ra-tile ra-transpose ra-from ra-order-c? c-dims
"
  (unless (positive? (ra-rank ra))
    (throw 'bad-rank-for-reshape (ra-rank ra)))
  (let ((sdims (apply c-dims s)))
    (let ((ssize (vector-fold (lambda (d c) (* c (dim-len d))) 1 sdims)))
      (when (> ssize (ra-length ra))
        (throw 'bad-size-for-reshape ssize (ra-length ra))))
    (match (vector-ref (%%ra-dims ra) 0)
      (($ <dim> ilen ilo istep)
       (let* ((sdims (vector-map (lambda (d) (make-dim (dim-len d) (dim-lo d) (* (dim-step d) istep))) sdims))
              (bdims (vector-append sdims (vector-drop (%%ra-dims ra) 1))))
         (make-ra-raw (%%ra-root ra)
                      (+ (ra-zero ra) (- (ra-offset 0 sdims)) (* ilo istep))
                      bdims))))))

(define (ra-tile ra . s)
  "
ra-tile ra s ... -> rb

Replicate ra RA by the shape S ... The shape of RB will be S concatenated with
the shape of RA.

Each element of S is either a list of two integers (LO HI) or an integer LEN.

The result always shares the root of RA.

See also: ra-ravel ra-reshape ra-transpose ra-from ra-order-c? c-dims
"
  (let ((ra (ra-check ra)))
    (make-ra-raw (%%ra-root ra) (%%ra-zero ra)
                 (vector-append
                  (vector-map (lambda (d) (make-dim (dim-len d) (dim-lo d) 0)) (apply c-dims s))
                  (%%ra-dims ra)))))
