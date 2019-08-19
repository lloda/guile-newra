; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2016-2017
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Trying things.

;; (import (newra newra) (newra tools) (newra base) (rnrs io ports)
;;         (srfi :8) (srfi :26) (ice-9 match) (srfi :1) (ice-9 format)
;;         (only (rnrs base) vector-map))

;; 
;; ; -----------------------
;; ; can't remember
;; ; -----------------------

;; (define ra0 (array->ra #(1 2 3)))
;; (define ra1 (array->ra #@1(1 2 3)))
;; (define ra2 (array->ra #2((1 2) (3 4))))
;; (define ra3 (array->ra #2@1@1((1 2) (3 4))))
;; (define ra4 (array->ra #3@1@1@-1(((1 2 3) (3 4 5)) ((4 5 6) (6 7 8)))))
;; (define ra5 (array->ra #0(99)))

;; (define v #(1 2 3 4))

;; (define (vector->list-forward v)
;;   (case (vector-length v)
;;     ((0) '())
;;     ((1) (list (vector-ref v 0)))
;;     (else
;;      (let ((first (list (vector-ref v 0))))
;;        (let loop ((last first)  (i 1))
;;          (if (= i (vector-length v))
;;            first
;;            (let ((next (list (vector-ref v i))))
;;              (set-cdr! last next)
;;              (loop next (+ i 1)))))))))


;; ,m (newra newra)

;; ; call macro with PARAM according to values OPT of TAG
;; (define-syntax %tag-dispatch
;;   (syntax-rules ()
;;     ((_ tag macro (opt ...) (param ...) args ...)
;;      (case tag ((opt) (macro param args ...)) ... (else (throw 'bad-tag tag))))))

;; (%tag-dispatch 'TWO display (ONE TWO) ('one 'two))

;; 
;; ; -----------------------
;; ; generalized selector
;; ; -----------------------

;; ; ...

;; 
;; ; -----------------------
;; ; define-inlinable-case-lambda
;; ; -----------------------

(import (newra newra) (newra tools) (rnrs io ports) (newra from)
        (srfi :8) (srfi :26) (ice-9 match) (only (srfi :1) fold iota)
        (only (rnrs base) vector-map))


; -----------------------
; ra-amend!
; -----------------------

; x m} y - https://code.jsoftware.com/wiki/Vocabulary/curlyrt#dyadic

(define amendu!
  (case-lambda
   ((A C)
    (ra-copy! A C))
   ((A C . ai)
    (let* ((ai (map (match-lambda ((? ra? x) x) ((? integer? x) (make-ra x))) ai))
           (bstairs (reverse (cdr (fold (lambda (a c) (cons (+ (ra-rank a) (car c)) c)) '(0) ai)))))
      (let ((frame (fold (lambda (a c) (+ c (ra-rank a))) 0 ai))
            (i (map (lambda (ai stairs)
                      (apply ra-transpose ai (iota (ra-rank ai) stairs)))
                 ai bstairs)))
        (if (= frame (ra-rank A) (ra-rank C))
; optimization
          (apply ra-map! A C i)
          (apply ra-slice-for-each frame
                 (lambda (C . i) (ra-copy! (apply (lambda i (apply ra-slice A i)) (map ra-ref i)) C))
                 C i))
        A)))))

(define beatable? (@@ (newra from) beatable?))
(define index-rank (@@ (newra from) index-rank))
(define vector-append (@@ (newra from) vector-append))
(define vector-drop (@@ (newra base) vector-drop))

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


; -----------------------------
; some cases ...
; -----------------------------

(define (ra-I . i) (ra-copy #t (apply ra-i i)))

(ra-amend! (ra-I 2 3) (array->ra #(a b c)) 0)
(ra-amend! (ra-I 2 3) (array->ra #(x y z)) 1)

(ra-amend! (ra-I 2 3) (array->ra #2((a b c))) (array->ra #(0)))
(ra-amend! (ra-I 2 3) (array->ra #2((x y z))) (array->ra #(1)))

(ra-amend! (ra-I 2 3) (array->ra #(a b)) #t 0)
(ra-amend! (ra-I 2 3) (array->ra #(x y)) #t 1)

(ra-amend! (ra-I 2 3) (array->ra #2((a b))) #t (array->ra #(0)))
(ra-amend! (ra-I 2 3) (array->ra #2((x y))) #t (array->ra #(1)))
