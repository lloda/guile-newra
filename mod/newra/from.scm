; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2018-2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Outer product and specialization to array views.
;;; Code:

; FIXME Simplify...

(define-module (newra from)
  #:export (ra-from ra-amend! fromb fromu amendu!)) ; testing

(import (newra base) (newra map) (newra lib) (srfi :8) (srfi :26) (srfi :1)
        (ice-9 control) (ice-9 match) (only (rnrs base) vector-map vector-for-each))

; -----------------------
; this is the pure beaten section.
; FIXME preallocate the dim vector then run j k ... j ...
; -----------------------

(define (fromb A . ai)
  (let loopj ((j 0) (ii ai) (zero (ra-zero A)) (bdims '()))
    (match ii
      ((i0 . irest)
       (let ((dimA (vector-ref (ra-dims A) j)))
         (match i0
           (#t
            (loopj (+ j 1) irest zero (cons (vector dimA) bdims)))
           ((? integer? z)
            (loopj (+ j 1) irest (+ zero (* (dim-step dimA) (dim-check dimA z))) bdims))
           ((? ra? i)
            (let ((ri (%%ra-rank i)))
              (if (zero? ri)
                (loopj (+ j 1) irest (+ zero (* (dim-step dimA) (dim-check dimA (i)))) bdims)
                (let ((root (%%ra-root i)))
                  (match root
                    (($ <aseq> rorg rinc)
                     (let ((bdimsj (make-vector ri))
                           (izero (%%ra-zero i)))
                       (let loopk ((k 0) (lo izero) (hi izero))
                         (if (< k ri)
                           (match (vector-ref (%%ra-dims i) k)
                             (($ <dim> ilen ilo istep)
                              (vector-set! bdimsj k (make-dim ilen ilo (* (dim-step dimA) istep rinc)))
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

                           (let ((lo (and lo (aseq-ref root lo)))
                                 (hi (and hi (aseq-ref root hi))))
                             (receive (lo hi) (if (negative? rinc) (values hi lo) (values lo hi))
; we don't use dim-check as that requires integer i.
                               (when (dim-lo dimA)
                                 (unless (and lo (>= lo (dim-lo dimA)))
                                   (throw 'dim-check-out-of-range dimA lo)))
                               (when (dim-hi dimA)
                                 (unless (and hi (<= hi (dim-hi dimA)))
                                   (throw 'dim-check-out-of-range dimA lo)))
                               (loopj (+ j 1) irest
                                      (+ zero (* (dim-step dimA) (+ rorg (* rinc izero))))
                                      (cons bdimsj bdims))))))))))))))))
      (()
       (make-ra-raw (ra-root A) zero
                    (vector-append (apply vector-append (reverse! bdims))
                                   (vector-drop (ra-dims A) (length ai))))))))


; ------------------------
; this is the pure unbeaten section
; ------------------------

; the empty case is needed so that (fromu A) shares the root of A, which is something (ra-from) relies on.
; fixme factor make-B out of fromu and the shared part of fromu amendu! and TBD fromu-copy!

(define (broadcast-indices . i)
  (let* ((frame (fold (lambda (i c) (+ c (ra-rank i))) 0 i))
         (i (map (lambda (i stairs)
                   (apply ra-transpose i (iota (ra-rank i) stairs)))
              i (reverse (cdr (fold (lambda (i c) (cons (+ (ra-rank i) (car c)) c)) '(0) i))))))
    (values frame i)))

; FIXME don't create the output here; show the symmetry with amendu!.

(define fromu
  (case-lambda
   ((A) A)
   ((A . i)
    (let ((C (make-ra-new
; type 'd needs to be converted
              (match (ra-type A) ('d #t) (x x))
              *unspecified*
              (apply c-dims
                (append (append-map ra-shape i)
                        (map (lambda (dim) (list (dim-lo dim) (dim-hi dim)))
                          (drop (vector->list (ra-dims A)) (length i))))))))
      (receive (frame i) (apply broadcast-indices i)
        (if (= frame (ra-rank A) (ra-rank C))
; optimization
          (apply ra-map! C A i)
          (apply ra-slice-for-each frame
                 (lambda (C . i) (ra-copy! C (apply (lambda i (apply ra-slice A i)) (map ra-ref i))))
                 C i))
        C)))))

(define amendu!
  (case-lambda
   ((A C)
    (ra-copy! A C))
   ((A C . i)
    (receive (frame i) (apply broadcast-indices i)
      (if (= frame (ra-rank A) (ra-rank C))
; optimization
        (apply ra-map! A C i)
        (apply ra-slice-for-each frame
               (lambda (C . i) (ra-copy! (apply (lambda i (apply ra-slice A i)) (map ra-ref i)) C))
               C i))
      A))))

(define (beatable? x)
  (or (and (ra? x) (or (zero? (ra-rank x)) (aseq? (ra-root x)))) (integer? x) (eq? x #t)))

(define (index-rank x)
  (match x ((? integer? z) 0) ((? ra? ra) (ra-rank ra)) (#t 1)))

(define (parse-args A . i)
  (let loop ((n 0) (m 0) (ii i)
             (ib '()) (ibi '()) (tb '())
             (iu '()) (iui '()) (tu '()))
    (match ii
      ((i0 . irest)
       (let* ((k (index-rank i0))
              (idest (iota k m)))
         (if (beatable? i0)
           (loop (+ n 1) (+ m k) irest
                 (cons i0 ib) (cons n ibi) (fold cons tb idest)
                 iu iui tu)
           (loop (+ n 1) (+ m k) irest
                 ib ibi tb
                 (cons i0 iu) (cons n iui) (fold cons tu idest)))))
      (()
       (let ((ib (reverse ib))
             (ibi (reverse ibi))
             (tb (reverse tb))
             (iu (reverse iu))
             (iui (reverse iui))
             (tu (reverse tu)))
; pick the beatable axes
         (let* ((B (make-ra-raw
                    (ra-root A) (ra-zero A)
                    (vector-map (cute vector-ref (ra-dims A) <>) (list->vector ibi))))
; beat them. This might change zero, but not root.
                (B (apply fromb B ib))
; put the unbeatable axes in front
                (B (make-ra-raw
                    (ra-root B) (ra-zero B)
                    (vector-append (vector-map (cute vector-ref (ra-dims A) <>) (list->vector iui))
                                   (ra-dims B)
                                   (vector-drop (ra-dims A) (length i))))))
           (values B iu tu tb)))))))

; FIXME add a version that copies the to an arg. That avoids allocation of the result, although it would be better if the compiler could tell where the result goes.

(define (ra-from A . i)
  "
ra-from a . i -> b

Outer product slice of A by indices I ...

The shape of B is the concatenation of the shapes of I, and the contents are
obtained by looking up in each dimension of A by the indices I, that is

B(i00 i01 ... i10 i11 ...) = A(i0(i00 i01 ...) i1(i10 i11 ...) ...)

where I : i0 i1 ...

The special value #t is understood as the full range of A on that axis.

Additionally, if every I is either 1) #t 2) a ra of type 'd, 3) a ra of rank 0,
or 4) an integer, the result B shares the root of A. In all other cases a new
root is allocated.

The type of B is the same as that of A, with the only exception that if the type
of A is 'd and the root of B isn't shared with the root of A, then the type of B
is #t.

See also: ra-cell ra-ref ra-slice ra-amend! ra-set!
"
  (receive (B iu tu tb) (apply parse-args A i)
; apply the unbeatable axes and undo the transposition. ra-transpose handles any trailing axes
    (apply ra-transpose (apply fromu B iu) (append tu tb))))


; -----------------------
; ra-amend!
; -----------------------

; x m} y - https://code.jsoftware.com/wiki/Vocabulary/curlyrt#dyadic

(define (ra-amend! A C . i)
  "
ra-amend! A C . i -> A

Copy C to the outer product slice of A by indices I ...

A(i0(i00 i01 ...) i1(i10 i11 ...) ...) <- C(i00 i01 ... i10 i11 ...)

where I : i0 i1 ...

This is equivalent to (ra-copy! (ra-from A I ...) C) whenever (ra-from A I ...)
would return a shared ra of A. I may take any of the special values accepted by
RA-FROM.

If I contains repeated indices so that the same elements of A are referenced
more than once, the value that ends up in A may correspond to any of the
indices.

This function returns the modified ra A.

See also: ra-set! ra-from ra-copy! ra-cell ra-ref ra-slice
"
  (receive (B iu tu tb) (apply parse-args A i)
; apply the unbeatable axes.
    (apply amendu! B (if (ra? C) C (make-ra C)) iu)
; we aren't making a new array so there's no need to transpose back.
    A))
