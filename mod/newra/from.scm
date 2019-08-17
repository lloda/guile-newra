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
  #:export (ra-from))

(import (newra base) (newra map) (newra lib) (srfi :8) (srfi :26) (srfi :1)
        (ice-9 control) (ice-9 match) (only (rnrs base) vector-map vector-for-each))

; -----------------------
; this is the pure beaten section.
; FIXME preallocate the dim vector then run j k ... j ...
; -----------------------

(define fromb
  (case-lambda
   ((A) A)
   ((A . ai)
    (let loopj ((j 0) (ii ai) (bzero (ra-zero A)) (bdims '()))
      (if (null? ii)
        (make-ra-raw (ra-root A) bzero
                     (vector-append (apply vector-append (reverse! bdims))
                                    (vector-drop (ra-dims A) (length ai))))
        (let ((dimA (vector-ref (ra-dims A) j)))
          (match (car ii)
            ((? ra? i)
             (match (ra-root i)
               (($ <dim> rlen rlo rstep)
                (let ((bdimsj (make-vector (ra-rank i))))
                  (let loopk ((k 0))
                    (if (= k (ra-rank i))
                      (loopj (+ j 1) (cdr ii)
                             (+ bzero (* (dim-step dimA) (- (+ rlo (* rstep (ra-zero i))) (dim-lo dimA))))
                             (cons bdimsj bdims))
                      (match (vector-ref (ra-dims i) k)
                        (($ <dim> ilen ilo istep)
                         (vector-set! bdimsj k (make-dim ilen ilo (* (dim-step dimA) istep rstep)))
                         (loopk (+ k 1))))))))
               (x
                (if (zero? (ra-rank i))
                  (loopj (+ j 1) (cdr ii)
                         (+ bzero (* (dim-step dimA) (- (i) (dim-lo dimA))))
                         bdims)
                  (throw 'not-yet x)))))
            ((? integer? z)
             (loopj (+ j 1) (cdr ii)
                    (+ bzero (* (dim-step dimA) (- z (dim-lo dimA))))
                    bdims)))))))))


; ------------------------
; this is the pure unbeaten section
; ------------------------

; the empty case is needed so that (fromu A) shares the root of A, which is something (ra-from) relies on.

(define fromu
  (case-lambda
   ((A) A)
   ((A . ai)
    (let* ((ai (map (match-lambda ((? ra? x) x) ((? integer? x) (make-ra x))) ai))
           (bshape
            (append
             (append-map ra-shape ai)
             (map (lambda (dim) (list (dim-lo dim) (dim-hi dim))) (drop (vector->list (ra-dims A)) (length ai)))))
           (bdims (apply c-dims bshape))
           (bstairs (reverse (cdr (fold (lambda (a c) (cons (+ (ra-rank a) (car c)) c)) '(0) ai))))
; FIXME type 'd needs to be converted
           (B (make-ra-new (ra-type A) *unspecified* bdims)))
      (let ((frame (fold (lambda (a c) (+ c (ra-rank a))) 0 ai))
            (i (map (lambda (ai stairs)
                      (apply ra-transpose ai (iota (ra-rank ai) stairs)))
                 ai bstairs)))
        (if (= frame (ra-rank A) (ra-rank B))
; optimization
          (apply ra-map! B A i)
          (apply ra-slice-for-each frame
                 (lambda (B . i) (ra-copy! B (apply (lambda i (apply ra-slice A i)) (map ra-ref i))))
                 B i))
        B)))))

(define (beatable? x)
  (or (and (ra? x) (or (zero? (ra-rank x)) (dim? (ra-root x)))) (integer? x)))
(define (index-rank x)
  (match x ((? integer? z) 0) ((? ra? ra) (ra-rank ra))))

(define (ra-from A . i)
  "
ra-from a . i -> b

Outer product slice of A by indices I ...

The shape of B is the concatenation of the shapes of I, and the contents are
obtained by looking up in each dimension of A by the indices I, that is

B(i00 i01 ... i10 i11 ...) = A(i0(i00 i01 ...) i1(i10 i11 ...) ...)

where I : i0 i1 ...

Additionally, if every one of the I is either 1) a ra of type 'd, 2) a ra of
rank 0, or 3) any integer, the result B is a view of the original array A over
the same root. In all other cases a new root is allocated.
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
