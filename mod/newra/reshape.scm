; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2018-2019, 2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Library for newra - manipulation of the dim vector
;;; Code:

(define-module (newra reshape)
  #:export (ra-reverse ra-transpose ra-untranspose ra-order-c?
            ra-ravel ra-reshape ra-tile
            ra-singletonize))

(import (newra base) (newra map) (newra vector)
        (ice-9 match) (ice-9 control) (srfi srfi-8) (srfi srfi-26)
        (only (srfi srfi-43) vector-copy!)
        (only (srfi srfi-1) fold every any iota drop first second)
        (only (rnrs base) vector-map vector-for-each))

(define (ra-reverse ra . k)
  "
Reverse axes @var{k} ... of array @var{ra}, 0 <= @var{k} < @code{(ra-rank
@var{ra})}.

The result shares the root of @var{ra}.

Example:

@lisp
  (ra-reverse (list->ra 2 '((0 1 2) (3 4 5))) 0 1)
  @result{} #%2:2:3((2 1 0) (5 4 3)))
@end lisp

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
Transpose each axis 0, 1, ... of @var{a} to matching destination @var{axes}.

An axis @var{k} of the result @var{b} may be referenced multiple times in
@var{axes} ... for source axes @var{i} ... . In that case the size of axis
@var{k} in the result is the smallest of the sizes of the source axes @var{i}
... , and the step of @var{k} is the sum of all the steps of @var{i} ... . The
lower bounds of @var{i} ... must all be the same.

Any axis of @var{b} that is not referenced in @var{axes} is a `dead' axis with
undefined dimension and step 0.

Axes of @var{ra} that aren't listed in @var{axes} are transposed to the end of
@var{b}'s axes.

The result shares the root of @var{ra}.

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
         (match ndim
           (#f odim)
           (($ <dim> nlen nlo nstep)
            (match odim
              (($ <dim> olen olo ostep)
               (if (= olo nlo)
                 (make-dim (if nlen (and olen (min nlen olen)) olen)
                           nlo (+ ostep nstep))
                 (throw 'bad-lo)))))))))))

; TODO Does it make sense to define this so rank ra = len axes, allow repeats?

(define (ra-untranspose rb . axes_)
  "
Transpose @var{axes} of @var{rb} to matching destination axes 0, 1, ... of the
result @var{ra}. This is the inverse of (ra-transpose @var{ra} @var{axes} ...)
-> @var{rb}.

@var{axes} must be a permutation of the list [0 ... (- (length @var{axes}) 1)]
and not be longer than the rank of @var{rb}. Each axis k = (@var{axes} i) of
@var{rb} is transposed to axis i = 0 ... (ra-rank @var{rb})-1 of @var{ra}. The
result has the same rank and the same root as @var{rb}.

The result shares the root of @var{rb}.

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

(define* (ra-order-c? ra #:optional n (org 0))
  "
Check whether axes [@var{org} .. @var{org}+@var{n}) of @var{ra} are in C-order
(aka row-major order).

An array frame is in C-order if the step of each axis is equal to the product of
the length and the step of the following axis. Axes with length 1 are
ignored. If any axis has length 0, the frame is automatically in C-order.

If @var{n} is not given, check whether the elements of @var{ra} are in packed
C-order. This means that 1) the full frame of RA is in C-order, and 2) on the
last axis, either the length is 1, or the length is 0, or the step is 1.

Single axes (@var{n} = 1) are always in C-order.

See also: ra-ravel ra-reshape ra-tile c-dims
"
  (let* ((ra (ra-check ra))
         (dims (%%ra-dims ra))
         (rank (vector-length dims))
         (end (or (and n (+ n org)) rank)))
    (unless (<= 0 end rank)
      (throw 'bad-axes-org-end-rank org end rank))
; look for an axis with len > 1
    (let loop ((i org) (step #f))
      (if (>= i end)
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

(define (pure-ravel ra n org)
  (let ((od (%%ra-dims ra))
        (rank (vector-length (%%ra-dims ra)))
        (end (+ n org)))
    (make-ra-root (%%ra-root ra)
; the ravel is based at 0, so we don't want (ra-zero ra).
                  (vector-append
                   (vector-take od org)
                   (vector (make-dim (ra-size ra n org) 0
                                     (cond ((zero? rank) 1)
                                           ((positive? n) (dim-step (vector-ref od (- end 1))))
                                           (else 1))))
                   (vector-drop od end))
                  (ra-offset (%%ra-zero ra) od n))))

; cf https://code.jsoftware.com/wiki/Vocabulary/comma

(define* (ra-ravel ra #:optional (n (ra-rank ra)) (org 0))
  "
Ravel axes [@var{org} .. @var{org}+@var{n}) of @var{ra} in C order. @var{n}
defaults to the rank of @var{ra} and @var{org} defaults to 0.

The result @var{rb} is an array with rank (rank(@var{ra}) - @var{n} + 1) that
may or may not share the root of RA.

When @var{rb} does not share the root of @var{ra}, then it has the same type as
@var{ra} unless that type is @code{d}, in which case it has type @code{#t}.

See also: ra-reshape ra-transpose ra-from ra-order-c?
"
  (pure-ravel
   (if (ra-order-c? ra n org)
     ra
     (ra-copy! (make-ra-new (match (%%ra-type ra) ('d #t) (x x))
                               *unspecified* (apply c-dims (ra-dimensions ra)))
               ra))
   n org))

; cf https://www.jsoftware.com/papers/APLDictionary1.htm#rho
; ... ⍺⍴⍵ produces a result of shape ⍺ from the elements of ⍵ ...

; cf https://code.jsoftware.com/wiki/Vocabulary/dollar#dyadic
; x $ y
; ... the shape of the result is (x,}.$y); i.e. the result is an array of items of y whose frame is x.

; cf http://www2.phys.canterbury.ac.nz/dept/docs/manuals/Fortran-90/HTMLNotesnode102.html#ReshapeIntrinsic1
; RESHAPE(SOURCE,SHAPE[,PAD][,ORDER])

(define (ra-reshape ra k . s)
  "
Reshape axis @var{k} of @var{ra} into shape @var{s} ... Each of the @var{s} is
either a list of two integers @code{(lo hi)} or an integer @code{len}.

The total size of the new axes must fit in axis @var{k} of @var{ra}. If @var{t}
is the shape of @var{ra}, the shape of the result is

@example
[t(0) ... t(k-1) s(0) ... t(k+1) ...]
@end example

At most one of the @var{s} may be @code{#f}. In that case, the missing length is
computed as @code{(/ (ra-len ra k) n)} where @var{n} is the total size of the
@var{s} that are not @code{#f}. It is an error if this isn't an integer ≥ 0.

The result shares the root of @var{ra}.

See also: ra-ravel ra-tile ra-transpose ra-from ra-order-c? c-dims make-ra-new
"
  (unless (positive? (ra-rank ra))
    (throw 'bad-rank-for-reshape (ra-rank ra)))
  (let* ((ph (fold (lambda (s c)
                     (match s
                       ((? integer? len)
                        (list (* len (first c)) (second c)))
                       (((? integer? lo) (? integer? hi))
                        (list (* (- hi lo -1) (first c)) (second c)))
                       (#f (if (second c)
                             (throw 'too-many-placeholders s)
                             (list (first c) #t)))))
                   '(1 #f) s))
         (s (if (second ph)
              (if (not (ra-len ra k))
                (throw 'cannot-use-placeholder-with-undefined-size ra s)
                (let ((len (/ (ra-len ra k) (first ph))))
                  (if (integer? len)
                    (map (lambda (s) (or s len)) s)
                    (throw 'bad-placeholder (ra-len ra k) (first ph) len))))
              s))
         (sdims (apply c-dims s)))
    (let ((ssize (vector-fold (lambda (d c) (* c (dim-len d))) 1 sdims)))
      (when (and (ra-len ra k) (> ssize (ra-len ra k)))
        (throw 'bad-size-for-reshape ssize (ra-len ra k))))
    (match (vector-ref (%%ra-dims ra) k)
      (($ <dim> ilen ilo istep)
       (let ((sdims (vector-map (match-lambda (($ <dim> dlen dlo dstep) (make-dim dlen dlo (* dstep istep))))
                                sdims)))
         (make-ra-root (%%ra-root ra)
                       (let* ((adims (%%ra-dims ra))
                              (bdims (make-vector (+ (vector-length sdims) (vector-length adims) -1))))
                         (vector-copy! bdims 0 adims 0 k)
                         (vector-copy! bdims k sdims)
                         (vector-copy! bdims (+ k (vector-length sdims)) adims (+ k 1))
                         bdims)
                       (+ (ra-zero ra) (- (ra-offset 0 sdims)) (* ilo istep))))))))

(define (ra-tile ra k . s)
  "
Replicate array @var{ra} by inserting axes of bounds @var{s} ... before axis
@var{k} of @var{ra}. If @var{t} is the shape of @var{ra}, the shape of the
result will be

@example
[t(0) ... t(k-1) s(0) ... t(k) ...]
@end example

Each of the @var{s} is either a list of two integers @code{(lo hi)} or an
integer @code{len}.

The result shares the root of @var{ra}.

See also: ra-ravel ra-reshape ra-transpose ra-from ra-order-c? c-dims
"
  (let ((ra (ra-check ra)))
    (make-ra-root (%%ra-root ra)
                  (let* ((sdims (vector-map (match-lambda (($ <dim> dlen dlo _) (make-dim dlen dlo 0)))
                                            (apply c-dims s)))
                         (adims (%%ra-dims ra))
                         (bdims (make-vector (+ (vector-length sdims) (vector-length adims)))))
                    (vector-copy! bdims 0 adims 0 k)
                    (vector-copy! bdims k sdims)
                    (vector-copy! bdims (+ k (vector-length sdims)) adims k)
                    bdims)
                  (%%ra-zero ra))))

(define (ra-singletonize ra . s)
  "
Set the lengths of any dead axes of @var{ra} (axes with step 0 and undefined
length) to 1.

The result shares the root of @var{ra}.

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
