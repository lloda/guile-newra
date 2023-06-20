; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2018-2019, 2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Outer product and specialization to array views.
;;; Code:

; FIXME Simplify...

(define-module (newra from)
  #:export (dots
            ra-from ra-from-copy ra-amend! ra-clip
            ra-rotate! ra-rotate
; FIXME these are tested separately in test/test.scm, which may not be necessary.
            fromb fromu amendu!))

(import (srfi srfi-1) (srfi srfi-9) (srfi srfi-9 gnu) (srfi srfi-26) (srfi srfi-71)
        (newra base) (newra map) (newra lib) (newra reshape) (newra vector)
        (ice-9 control) (ice-9 match)
        (only (srfi srfi-43) vector-copy)
        (only (rnrs base) vector-map vector-for-each))

(define-immutable-record-type <dots>
  (dots* n) dots?
  (n dots-n))

(define* (dots #:optional n)
  (if (or (not n) (and (>= n 0) (exact-integer? n)))
    (dots* n)
    (throw 'bad-argument-to-dots)))

; -----------------------
; this is the pure beaten section.
; FIXME preallocate the dim vector then run j k ... j ...
; -----------------------

(define (count-axes-left x)
  (fold (lambda (x n)
          (+ n
             (match x
               ((? dots? x) (or (dots-n x) (throw 'expanding-dots-used-more-than-once)))
               (else 1))))
        0 x))

(define (fromb A . ai)
  (let loopj ((j 0) (ii ai) (zero (ra-zero A)) (bdims '()))
    (match ii
      (()
       (make-ra-root (ra-root A)
                     (vector-append (apply vector-append (reverse! bdims))
                                    (vector-drop (%%ra-dims A) j))
                     zero))
      ((i0 . irest)
       (match i0
         (#t
          (loopj (+ j 1) irest zero (cons (vector (vector-ref (%%ra-dims A) j)) bdims)))
         ((? integer? z)
          (match (vector-ref (%%ra-dims A) j)
            (($ <dim> Alen Alo Astep)
             (loopj (+ j 1) irest (+ zero (* Astep (dim-check Alen Alo z))) bdims))))
         (($ <dots> n)
          (let ((jnext (+ j n)))
            (unless (<= jnext (%%ra-rank A))
              (throw 'dots-n-at-j-too-large-for-rank n j (%%ra-rank A)))
            (loopj jnext irest zero (cons (vector-copy (%%ra-dims A) j jnext) bdims))))
         ((? ra? i)
          (match (vector-ref (%%ra-dims A) j)
            (($ <dim> Alen Alo Astep)
             (let ((ri (%%ra-rank i)))
               (if (zero? ri)
                 (loopj (+ j 1) irest (+ zero (* Astep (dim-check Alen Alo (i)))) bdims)
                 (let ((root (%%ra-root i)))
                   (match root
                     (($ <aseq> rorg rinc)
                      (let ((bdimsj (make-vector ri))
                            (izero (%%ra-zero i)))
                        (let loopk ((k 0) (lo izero) (hi izero))
                          (if (< k ri)
                            (match (vector-ref (%%ra-dims i) k)
                              (($ <dim> ilen ilo istep)
                               (vector-set! bdimsj k (make-dim ilen ilo (* Astep istep rinc)))
                               (cond
                                ((zero? istep)
                                 (loopk (+ k 1) lo hi))
                                ((positive? istep)
                                 (loopk (+ k 1)
                                        (and lo ilo (+ lo (* istep ilo)))
                                        (and hi ilo ilen (+ hi (* istep (+ ilo ilen -1))))))
                                (else
                                 (loopk (+ k 1)
                                        (and lo ilo ilen (+ lo (* istep (+ ilo ilen -1))))
                                        (and hi ilo (+ hi (* istep ilo))))))))

                            (let* ((lo (and lo (aseq-ref root lo)))
                                   (hi (and hi (aseq-ref root hi)))
                                   (lo hi (if (negative? rinc) (values hi lo) (values lo hi))))
; don't use dim-check as that requires integer i.
                              (when Alo
                                (unless (and lo (>= lo Alo))
                                  (throw 'dim-check-out-of-range Alen Alo lo)))
                              (when Alen ; implies Alo
                                (unless (and hi (<= hi (dim-hi Alen Alo)))
                                  (throw 'dim-check-out-of-range Alen Alo hi)))
                              (loopj (+ j 1) irest
                                     (+ zero (* Astep (+ rorg (* rinc izero))))
                                     (cons bdimsj bdims)))))))))))))))))))


; ------------------------
; this is the pure unbeaten section
; ------------------------

; the empty case is needed so that (fromu A) shares the root of A, which is something (ra-from) relies on.
; FIXME factor make-B out of fromu and the shared part of fromu amendu! and TBD fromu-copy!

(define (broadcast-indices i)
  (let* ((frame (fold (lambda (i c) (+ c (ra-rank i))) 0 i))
         (i (map (lambda (i stairs)
                   (apply ra-transpose i (iota (ra-rank i) stairs)))
              i (reverse (cdr (fold (lambda (i c) (cons (+ (ra-rank i) (car c)) c)) '(0) i))))))
    (values frame i)))

; FIXME don't create the output here; show the symmetry with amendu!.
(define fromu
  (case-lambda
; optimization: no indices
   ((A) A)
   ((A . i)
    (let ((C (make-ra-new
; type 'd needs to be converted
              (match (ra-type A) ('d #t) (x x))
              *unspecified*
              (apply c-dims
                (append (append-map ra-shape i)
                        (map (match-lambda (($ <dim> len lo _) (list lo (dim-hi len lo))))
                          (drop (vector->list (ra-dims A)) (length i)))))))
          (frame i (broadcast-indices i)))
      (if (= frame (ra-rank C))
; optimization: scalar dest
        (apply ra-map! C A i)
        (apply ra-slice-for-each
          frame
          (lambda (C . i) (ra-copy! C (apply (lambda (i) (ra-slice A (ra-ref i))) i)))
          C i))
      C))))

(define amendu!
  (case-lambda
; optimization: no indices
   ((A C) (ra-copy! A C))
   ((A C . i)
    (let ((frame i (broadcast-indices i)))
      (if (= (ra-rank A) (length i))
; optimization: scalar dest
        (apply ra-for-each (cut ra-set! A <> <...>) C i)
        (apply ra-slice-for-each
          frame
          (lambda (C . i) (ra-copy! (apply (lambda (i) (ra-slice A (ra-ref i))) i) C))
          C i))
      A))))

(define (beatable? x)
  (or (and (ra? x) (or (zero? (ra-rank x)) (aseq? (ra-root x)))) (integer? x) (eq? x #t)))

(define (index-rank x)
  (match x
    ((? integer? z) 0)
    ((? ra? ra) (ra-rank ra))
    (#t 1)
    (else (throw 'bad-index x))))

; split the beatable part of A and chew the indices for fromb.
; FIXME going over the args twice, here and in fromb

(define (parse-args A . i)
  (let loop ((j 0) (m 0) (ii i)
             (ib '()) (ibi '()) (tb '())
             (iu '()) (iui '()) (tu '()))
    (match ii
      ((i0 . irest)
       (match i0
         (($ <dots> n)
          (let* ((k (or n (- (ra-rank A) j (count-axes-left irest))))
                 (idest (iota k m)))
            (loop (+ j k) (+ m k) irest
                  (cons (dots k) ib) (fold cons ibi (iota k j)) (fold cons tb idest)
                  iu iui tu)))
         (i0
          (let* ((k (index-rank i0))
                 (idest (iota k m)))
            (if (beatable? i0)
              (loop (+ j 1) (+ m k) irest
                    (cons i0 ib) (cons j ibi) (fold cons tb idest)
                    iu iui tu)
              (loop (+ j 1) (+ m k) irest
                    ib ibi tb
                    (cons i0 iu) (cons j iui) (fold cons tu idest)))))))
      (()
       (when (> j (%%ra-rank A))
         (throw 'too-many-indices-for-rank-of-A j (%%ra-rank A)))
       (let ((ib (reverse! ib))
             (ibi (reverse! ibi))
             (iu (reverse! iu))
             (iui (reverse! iui))
             (tub (reverse! (append! tb tu))))
; pick the beatable axes
         (let* ((B (make-ra-root
                    (ra-root A)
                    (vector-map (cute vector-ref (ra-dims A) <>) (list->vector ibi))
                    (ra-zero A)))
; beat them. This might change zero, but not root.
                (B (apply fromb B ib))
; put the unbeatable axes in front
                (B (make-ra-root
                    (ra-root B)
                    (vector-append (vector-map (cute vector-ref (ra-dims A) <>) (list->vector iui))
                                   (ra-dims B)
                                   (vector-drop (ra-dims A) j))
                    (ra-zero B))))
           (values B iu tub)))))))

; FIXME add a version that copies the result to an arg. That avoids allocation of the result, although it would be better if the compiler could tell where the result goes.

(define (ra-from* copy? A . i)
  (let ((B iu tub (apply parse-args A i)))
; optimization.
    (if (null? iu)
      (if copy? (ra-copy B) B)
; apply the unbeatable axes and undo the transposition. ra-transpose handles any trailing axes
      (apply ra-transpose (apply fromu B iu) tub))))

(define (ra-from A . i)
  "
Outer product slice of @var{A} by indices @var{i} ...

The shape of the result @var{B} is the concatenation of the shapes of
@var{i}... and the contents are obtained by looking up in each dimension of
@var{A} by the indices @var{i}, that is

@verbatim
B(i00 i01 ... i10 i11 ...) = A(i0(i00 i01 ...) i1(i10 i11 ...) ...)
@end verbatim

where @var{i} : i0 i1 ...

The special value #t is understood as the full range of @var{A} on that axis.

Additionally, if every @var{i} ... is either 1) #t 2) an array of type
@code{'d}, 3) an array of rank 0, or 4) an integer, the result @var{B} shares
the root of @var{A}. In all other cases a new root is allocated.

The type of the result @var{B} is the same as that of @var{A}, with the
exception that if the type of @var{A} is @code{'d} and the root of @var{B} isn't shared
with the root of @var{A}, then the type of @var{B} is @code{#t}.

See also: @code{ra-cell} @code{ra-ref} @code{ra-slice} @code{ra-amend!} @code{ra-set!}
"
  (apply ra-from* #f A i))

(define (ra-from-copy A . i)
  "
Like @code{(ra-from A i ...)}, but always return a newly allocated array.

See also: @code{ra-from} @code{ra-amend!} @code{ra-copy} @code{ra-copy!}
"
  (apply ra-from* #t A i))


; -----------------------
; ra-amend!
; -----------------------

; x m} y - https://code.jsoftware.com/wiki/Vocabulary/curlyrt#dyadic

(define (ra-amend! A C . i)
  "
Copy @var{C} to the outer product slice of @var{A} by indices var{i} ...

@verbatim
A(i0(j00 j01 ...) i1(j10 j11 ...) ...) <- C(j00 j01 ... j10 j11 ...)
@end verbatim

where @var{i} : i0 i1 ...

This is equivalent to @code{(ra-copy! (ra-from A i ...) C)} whenever
@code{(ra-from A i ...)} would return a view of @var{A}. @var{i} may take any of
the special values accepted by @code{ra-from}.

The copy is performed in no particular order.  If @var{i} contains repeated
indices or the steps of @var{A} make it so that the same elements of the root of
@var{A} are referenced more than once, the value that ends up in @var{A} may
correspond to any of the indices that match those elements.

This function returns the modified array @var{A}.

See also: @code{ra-set!} @code{ra-from} @code{ra-copy!} @code{ra-cell} @code{ra-ref} @code{ra-slice}
"
  (let* ((B iu tub (apply parse-args A i))
; C needs to be transposed to match the transposition of B relative to A.
         (C (if (ra? C)
              (apply ra-untranspose C (take tub (min (length tub) (ra-rank C))))
              (make-ra C))))
; apply the unbeatable axes.
    (apply amendu! B C iu)
; not making a new array so there's no need to transpose back.
    A))


; -----------------------
; derived in some way from ra-from
; -----------------------

; ra-from resets the bounds so it cannot be reused here.
(define (ra-clip a b)
  "
Slice A to the intersection of the bounds of A and B.

See also: @code{ra-from} @code{ra-amend!} @code{ra-reshape}
"
  (let ((db (ra-dims b))
        (da (vector-copy (ra-dims a))))
    (let loop ((i (- (min (vector-length da) (vector-length db)) 1)))
      (if (negative? i)
        (make-ra-root (ra-root a) da (ra-zero a))
        (match (vector-ref da i)
          (($ <dim> alen alo astep)
           (match (vector-ref db i)
             (($ <dim> blen blo bstep)
              (let* ((lo (max alo blo))
                     (hi (min (+ alo alen -1) (+ blo blen -1))))
                (vector-set! da i (make-dim (max 0 (- hi lo -1)) lo astep))
                (loop (- i 1)))))))))))


; -----------------------
; ra-rotate! in place,  first axis left (cf 2⌽l←'abcdef' ⇒ cdefab)
; -----------------------

; based on libstdc++ stl_algo.h _RandomAccessIterator __rotate.
; FIXME maybe move to (lib extra) or so. Doesn't fit here.
; FIXME custom case for k = ±1.
; FIXME replace ra-from calls by bumps of zero.

(define (ra-rotate! n a)
  "
Rotate in place the first axis of array @var{a} by @var{n} positions, and return
the modified @var{a}. For example:

@lisp
(define a (ra-copy #t (ra-i 3 2)))
(ra-rotate! 1 a)
@result {} #%1:3:2((2 3) (4 5) (0 1))
@end lisp

See also: @code{ra-rotate}, @code{ra-reverse}
"
  (let* ((a (ra-check a))
         (rank (%%ra-rank a)))
    (match (vector-ref (%%ra-dims a) 0)
      (($ <dim> s lo step)
       (if (zero? s)
         a
         (let loop ((p lo) (n (euclidean-remainder n s)) (s s))
           (cond
            ((zero? n) a)
            ((= (* 2 n) s)
             (ra-swap!
              (ra-from a (ra-iota n p))
              (ra-from a (ra-iota n (+ p n))))
             a)
            ((< (* 2 n) s)
             (ra-swap-in-order!
              (ra-from a (ra-iota (- s n) p))
              (ra-from a (ra-iota (- s n) (+ p n))))
             (let ((p (+ p (- s n)))
                   (s (euclidean-remainder s n)))
               (if (positive? s)
                 (loop p (- n s) n)
                 a)))
            (else
             (let ((n (- s n)))
               (ra-swap-in-order!
                (ra-from a (ra-iota (- s n) (+ p s -1) -1))
                (ra-from a (ra-iota (- s n) (+ p s -1 (- n)) -1)))
               (loop p (euclidean-remainder s n) n))))))))))

; FIXME surely we can do better

(define (ra-rotate n a)
  "
Like @code{ra-rotate!}, but return a new array instead of modifying the input
array @var{a}.

See also: @code{ra-rotate!}, @code{ra-reverse}
"
  (ra-rotate! n (ra-copy a)))
