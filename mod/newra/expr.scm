; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Laziness and composition for newra, WIP.
;;; Code:

;; (define-module (newra expr))

(import (newra) (only (srfi :1) fold every) (srfi :71) (srfi :26) (ice-9 match))


; ----------------
; 1.a verbs w/o return values
; ----------------

; fields are: [0:apply 1:op 2:[ranks ...] 3:[op [ranks ...]] ...]
(define <verb-vtable>
  (make-struct/no-tail
   <applicable-struct-vtable>
   (make-struct-layout (string-append "pwpwpwpw"))))

(define-inlinable (verb? o)
  (and (struct? o) (eq? <verb-vtable> (struct-vtable o))))

(define (verb-or-proc? o)
  (or (verb? o) (procedure? o)))

(define-syntax %%struct-ref (syntax-rules () ((_ a n) (struct-ref a n))))
(define-inlinable (%%verb-op a) (%%struct-ref a 1))
(define-inlinable (%%verb-ranks a) (%%struct-ref a 2))
(define-inlinable (%%verb-implementation-op-ranks a) (%%struct-ref a 3))

(define-syntax %verbstruct-ref (syntax-rules () ((_ a n) (begin (unless (verb? a) (throw 'not-a-verb a)) (struct-ref a n)))))
(define-inlinable (%verb-op v) (%verbstruct-ref v 1))
(define-inlinable (%verb-ranks v) (%verbstruct-ref v 2))
(define-inlinable (%verb-implementation-op-ranks v) (%verbstruct-ref v 3))

(define (verb-print v port)
  (format #t "<verb op: ~a ranks: ~a alt: ~a>" (%verb-op v) (%verb-ranks v) (%verb-implementation-op-ranks v)))

(struct-set! <verb-vtable> vtable-index-printer verb-print)

(define* (ranks? ranks)
  (every (lambda (k) (or (integer? k) (eq? '∞ k) (eq? 'infty k))) ranks))

(define* (ranks-final? ranks)
  (every (lambda (k) (and (integer? k) (>= k 0))) ranks))

(define (make-verb op . ranks)
  (unless (ranks? ranks) (throw 'bad-ranks ranks))
  (unless (verb-or-proc? op) (throw 'bad-op op))
  (make-struct/no-tail
             <verb-vtable>
             (lambda i (throw 'you-tried-to-call op ranks i))
             op ranks '()))

(define (make-verb-alternates op-ranks0 . op-ranks)
  (match (cons op-ranks0 op-ranks)
    (((op0 ranks0) (op ranks) ...)
; alternates doesn't accept argument-dependent rank.
     (unless (verb-or-proc? op0) (throw 'bad-op op0))
     (unless (ranks-final? ranks0) (throw 'bad-ranks-final ranks0))
     (let check ((ranks0 ranks0) (ranks ranks) (op op))
       (unless (null? ranks)
         (unless (verb-or-proc? (car op))
           (throw 'bad-alternate-op (car op)))
; TODO maybe it's better to select an alternates differently. Like we may have 0 0, then 0 1, then 1 0. So there is no order. So we could pick an alternate by the smallest Σ (verb rank - arg rank) for example.
         (unless (and (= (length ranks0) (length (car ranks)))
                      (every <= ranks0 (car ranks)))
           (throw 'bad-alternate-ranks (car ranks)))
         (check (car ranks) (cdr ranks) (cdr op))))
; TODO maybe we want to store the alternates differently? probably want to match them last to first.
     (make-struct/no-tail
      <verb-vtable>
      (lambda i (throw 'you-tried-to-call-alternates op0 ranks0 i))
      op0 ranks0 op-ranks))))

(define w/rank )

(define +ᵤ (make-verb (lambda (a b c) (ra-set! c (+ (ra-ref a) (ra-ref b)))) 0 0 0))
(define +ᵘ (make-verb-alternates (list (lambda (a b c) (ra-set! c (+ (ra-ref a) (ra-ref b)))) (list 0 0 0))))


; ----------------
; 1.b application (rank extension) of verbs w/o return values.
; ----------------

; we'll do this using ra-transpose and dead axes - it's more obvious than the adhoc mechanism in guile-ploy.
; (verb-with-ranks A ...) -> (ra-slice-for-each naked-op (transpose-appropriately A) ...)


; ----------------
; 2.c composition of verbs w/o return values.
; ----------------


; ----------------
; 2.a return values.
; ----------------


; ----------------
; 2.b application with return values
; ----------------


; ----------------
; 2.c composition with return values
; ----------------
