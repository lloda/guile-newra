
; Replacement for Guile C-based array system - WIP
; (c) Daniel Llorens - 2016-2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(import (newra newra) (newra print) (newra tools)
        (only (rnrs base) vector-map) (only (srfi srfi-1) fold)
        (rnrs io ports) (srfi srfi-8)
        (srfi srfi-26) (ice-9 match))

(define ra0 (array->ra #(1 2 3)))
(define ra1 (array->ra #@1(1 2 3)))
(define ra2 (array->ra #2((1 2) (3 4))))
(define ra3 (array->ra #2@1@1((1 2) (3 4))))
(define ra4 (array->ra #3@1@1@-1(((1 2 3) (3 4 5)) ((4 5 6) (6 7 8)))))
(define ra5 (array->ra #0(99)))

(define v #(1 2 3 4))

(define (vector->list-forward v)
  (case (vector-length v)
    ((0) '())
    ((1) (list (vector-ref v 0)))
    (else
     (let ((first (list (vector-ref v 0))))
       (let loop ((last first)  (i 1))
         (if (= i (vector-length v))
           first
           (let ((next (list (vector-ref v i))))
             (set-cdr! last next)
             (loop next (+ i 1)))))))))


(define-syntax %%rastruct-ref (syntax-rules () ((_ a n) (struct-ref a n))))

; call macro with PARAM according to values OPT of TAG
(define-syntax %tag-dispatch
  (syntax-rules ()
    ((_ tag macro (opt ...) (param ...) args ...)
     (case tag ((opt) (macro param args ...)) ... (else (throw 'bad-tag tag))))))

(%tag-dispatch 'TWO display (ONE TWO) ('one 'two))

(define (ra-foll! ra fill)
  (letrec-syntax
      ((%opper-fill!
        (syntax-rules ()
          ((_ %op fill ra vset)
           (vset (%%ra-data ra) (%%ra-zero ra) fill))))
       (%slice-loop-type
        (syntax-rules ()
          ((_ (ra-rank ra) fill
              %opper-fill! %op %list %let
              %stepu %stepu-back %stepk %stepk-back (ra) (vset))
           (%slice-loop ))))
    (%tag-dispatch tag
                   (%slice-loop-type
    ra)))))

(define (ra-fill! ra fill)
  (let-syntax
      ((%opper-fill!
        (syntax-rules ()
          ((_ %op fill ra)
           ((%%ra-vset! ra) (%%ra-data ra) (%%ra-zero ra) fill)))))
    (%slice-loop (ra-rank ra) fill
                 %opper-fill! %op %list %let
                 %stepu %stepu-back %stepk %stepk-back (ra))
    ra))
