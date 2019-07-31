; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2016-2017
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Trying things.

(import (newra newra) (newra print) (newra tools)
        (only (rnrs base) vector-map) (only (srfi srfi-1) fold)
        (rnrs io ports) (srfi srfi-8)
        (srfi srfi-26) (ice-9 match))


; -----------------------
; can't remember
; -----------------------

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


,m (newra newra)

; call macro with PARAM according to values OPT of TAG
(define-syntax %tag-dispatch
  (syntax-rules ()
    ((_ tag macro (opt ...) (param ...) args ...)
     (case tag ((opt) (macro param args ...)) ... (else (throw 'bad-tag tag))))))

(%tag-dispatch 'TWO display (ONE TWO) ('one 'two))


; -----------------------
; prefix matching
; -----------------------

(define a0 (make-ra 0 2 3 4))
(define a1 (make-ra 0 2 3))

(ra-for-each (lambda (a b) 3) a0 a0)
(ra-for-each (lambda (a b) 3) a0 a1)
(ra-for-each! a0 - a1)
