; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2018-2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Library for Newra - manipulation of the dim vector
;;; Code:

(define-module (newra lib reshape)
  #:export (ra-reverse ra-transpose ra-untranspose ra-order-c?
            ra-ravel ra-reshape ra-tile
            ra-singletonize))

(import (newra base) (newra map) (only (srfi :1) fold every any iota drop) (srfi :8) (srfi :26)
        (ice-9 control) (ice-9 match) (only (rnrs base) vector-map vector-for-each))

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
        (make-ra-root (%%ra-root ra) ndims zero)
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
         (make-ra-root (%%ra-root ra)
; append rest axes
                       (vector-append ndims (vector-drop odims i))
                       (%%ra-zero ra)))
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

(define (ra-untranspose rb . axes_)
  "
ra-untranspose rb axes ... -> ra

Reverse the transposition (ra-transpose ra axes ...).

AXES must be a permutation of the list [0 ... (-1 (length axes))] and not be
longer than the rank of RB. Each axis k = (AXES i) of RB is transposed to axis i
= 0 ... (rak-rank rb)-1 of RA. The result has the same rank and the same root as
the argument.

See also: ra-transpose ra-dims
"
  (let* ((ra (ra-check rb))
         (odims (%%ra-dims rb))
         (ndims (make-vector (vector-length odims) #f)))
    (let loop ((n 0) (m -1) (axes axes_))
      (if (null? axes)
        (if (= n (vector-length odims))
          (make-ra-root (ra-root ra) ndims (ra-zero ra))
          (if (= (+ 1 m) n)
; axes is short but complete, so the rest isn't transposed. Just copy it.
            (let loop ((n n))
              (if (= n (vector-length odims))
                (make-ra-root (ra-root ra) ndims (ra-zero ra))
                (begin
                  (vector-set! ndims n (vector-ref odims n))
                  (loop (+ n 1)))))
            (throw 'bad-untranspose-axes ra n m axes_)))
        (let* ((o (car axes))
               (d (vector-ref odims o)))
          (if (vector-ref ndims n)
            (throw 'bad-untranspose-axes ra axes_)
            (vector-set! ndims n (vector-ref odims o)))
          (loop (+ n 1) (max m o) (cdr axes)))))))

(define* (ra-order-c? ra #:optional n)
  "
ra-order-c? ra
ra-order-c? ra n

Check whether the N-frame of RA is in C-order (aka row-major order).

An ra frame is in C-order if the step of each axis is equal to the product of
the length and the step of the following axis. Axes with length 1 are
ignored. If any axis has length 0, the frame is in C-order.

If N is not given, check whether the elements of RA are in packed C-order. This
means that 1) the full frame of RA is in C-order, and 2) on the last axis,
either the length is 1, or the length is 0, or the step is 1.

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
      (make-ra-root (%%ra-root ra)
; the ravel is based at 0, so we don't want (ra-zero ra).
                    (vector-append
                     (vector (make-dim (ra-size ra n) 0
                                       (cond ((zero? rank) 1)
                                             ((positive? n) (dim-step (vector-ref od (- n 1))))
                                             (else 1))))
                     (vector-drop od n))
                    (ra-offset (%%ra-zero ra) od n))))
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
      (when (and (ra-len ra) (> ssize (ra-len ra)))
        (throw 'bad-size-for-reshape ssize (ra-len ra))))
    (match (vector-ref (%%ra-dims ra) 0)
      (($ <dim> ilen ilo istep)
       (let* ((sdims (vector-map (lambda (d) (make-dim (dim-len d) (dim-lo d) (* (dim-step d) istep))) sdims))
              (bdims (vector-append sdims (vector-drop (%%ra-dims ra) 1))))
         (make-ra-root (%%ra-root ra)
                       bdims
                       (+ (ra-zero ra) (- (ra-offset 0 sdims)) (* ilo istep))))))))

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
    (make-ra-root (%%ra-root ra)
                  (vector-append
                   (vector-map (lambda (d) (make-dim (dim-len d) (dim-lo d) 0)) (apply c-dims s))
                   (%%ra-dims ra))
                  (%%ra-zero ra))))

(define (ra-singletonize ra . s)
  "
ra-singletonize ra -> rb

Set the lengths of any dead axes of RA (axes with step 0) to 1.

See also: ra-transpose
"
  (make-ra-root
   (ra-root ra)
   (vector-map (lambda (dim)
                 (match dim
                   (($ <dim> len lo step)
                    (make-dim (cond (len len)
                                    ((zero? step) 1)
                                    (else (throw 'cannot-singletonize dim)))
                              (or lo 0)
                              step))))
               (ra-dims ra))
   (ra-zero ra)))
