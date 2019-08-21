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

; lowest and highest positions on data.
(define (%ra-pos-bounds zero dims)
  (let loop ((j (- (vector-length dims) 1)) (lo zero) (hi zero))
    (if (< j 0)
      (values lo hi)
      (let* ((dim (vector-ref dims j))
             (step (dim-step dim)))
        (cond
         ((zero? step)
          (loop (- j 1) lo hi))
         ((positive? step)
          (loop (- j 1) (+ lo (* step (dim-lo dim))) (+ hi (* step (dim-hi dim)))))
         (else
          (loop (- j 1) (+ lo (* step (dim-hi dim))) (+ hi (* step (dim-lo dim))))))))))

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
                    (($ <dim> rlen rlo rstep)
                     (let ((bdimsj (make-vector ri))
                           (izero (%%ra-zero i)))
                       (let loopk ((k 0) (lo izero) (hi izero))
                         (if (< k ri)
                           (match (vector-ref (%%ra-dims i) k)
                             (($ <dim> ilen ilo istep)
                              (vector-set! bdimsj k (make-dim ilen ilo (* (dim-step dimA) istep rstep)))
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

                           (let ((lo (and lo (dim-ref root lo)))
                                 (hi (and hi (dim-ref root hi))))
                             (receive (lo hi) (if (negative? rstep) (values hi lo) (values lo hi))
; we don't use dim-check as that requires integer i.
                               (when (dim-lo dimA)
                                 (unless (and lo (>= lo (dim-lo dimA)))
                                   (throw 'dim-check-out-of-range dimA lo)))
                               (when (dim-hi dimA)
                                 (unless (and hi (<= hi (dim-hi dimA)))
                                   (throw 'dim-check-out-of-range dimA lo)))
                               (loopj (+ j 1) irest
                                      (+ zero (* (dim-step dimA) (+ rlo (* rstep izero))))
                                      (cons bdimsj bdims))))))))))))))))
      (()
       (make-ra-raw (ra-root A) zero
                    (vector-append (apply vector-append (reverse! bdims))
                                   (vector-drop (ra-dims A) (length ai))))))))


; ------------------------
; this is the pure unbeaten section
; ------------------------

; the empty case is needed so that (fromu A) shares the root of A, which is something (ra-from) relies on.

(define fromu
  (case-lambda
   ((A) A)
   ((A . ai)
    (let* ((bshape
            (append
             (append-map ra-shape ai)
             (map (lambda (dim) (list (dim-lo dim) (dim-hi dim))) (drop (vector->list (ra-dims A)) (length ai)))))
           (bdims (apply c-dims bshape))
; FIXME type 'd needs to be converted
           (B (make-ra-new (ra-type A) *unspecified* bdims))
           (bstairs (reverse (cdr (fold (lambda (a c) (cons (+ (ra-rank a) (car c)) c)) '(0) ai))))
           (i (map (lambda (ai stairs)
                     (apply ra-transpose ai (iota (ra-rank ai) stairs)))
                ai bstairs))
           (frame (fold (lambda (a c) (+ c (ra-rank a))) 0 ai)))
        (if (= frame (ra-rank A) (ra-rank B))
; optimization
          (apply ra-map! B A i)
          (apply ra-slice-for-each frame
                 (lambda (B . i) (ra-copy! B (apply (lambda i (apply ra-slice A i)) (map ra-ref i))))
                 B i))
        B))))

(define amendu!
  (case-lambda
   ((A C)
    (ra-copy! A C))
   ((A C . ai)
    (let* ((bstairs (reverse (cdr (fold (lambda (a c) (cons (+ (ra-rank a) (car c)) c)) '(0) ai))))
           (i (map (lambda (ai stairs)
                     (apply ra-transpose ai (iota (ra-rank ai) stairs)))
                ai bstairs))
           (frame (fold (lambda (a c) (+ c (ra-rank a))) 0 ai)))
        (if (= frame (ra-rank A) (ra-rank C))
; optimization
          (apply ra-map! A C i)
          (apply ra-slice-for-each frame
                 (lambda (C . i) (ra-copy! (apply (lambda i (apply ra-slice A i)) (map ra-ref i)) C))
                 C i))
        A))))

(define (beatable? x)
  (or (and (ra? x) (or (zero? (ra-rank x)) (dim? (ra-root x)))) (integer? x) (eq? x #t)))
(define (index-rank x)
  (match x ((? integer? z) 0) ((? ra? ra) (ra-rank ra)) (#t 1)))

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

See also: ra-cell ra-ref ra-slice ra-amend! ra-set!
"
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
                                   (vector-drop (ra-dims A) (length i)))))
; apply them.
                (B (apply fromu B iu)))
; undo the transposition. ra-transpose handles any trailing axes
           (apply ra-transpose B (append tu tb))))))))


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
more than once, the value that ends up in A could correspond to any of the
indices.

This function returns the modified ra A.

See also: ra-set! ra-from ra-copy! ra-cell ra-ref ra-slice
"
  (let ((C (if (ra? C) C (make-ra C))))
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
; up to now this is the same as ra-from.
; but we aren't making a new array so there's no need to transpose back.
             (apply amendu! B C iu)
             A)))))))
