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

(import (newra newra) (only (srfi :1) fold every) (srfi :71) (srfi :26) (ice-9 match))


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

; 0 ra-slice-for-each. No need of array-map/frame! as opₐ doesn't return values.
; 1 nested-op-frames.
;   unlike in (ploy ploy), here we need to look out for alternates.
;   still need to support the rank conjunction mechanism.
;   we always start at k=0.

; Rank-extend A with cell rank r to frame f.
; [(a skipped k prefix) ( (-r)-frame of a/k... ) ### (a/k r-cell ...)]
; [(a skipped k prefix) ( f ...................... ) (a/k r-cell ...)]
; where ### are the extended axes.
; TODO the scalar cases should be handled specially so that this isn't needed.
(define (match-frame a f r k)
  "Rank-extend a with cell rank r to frame f."
  (cond ((length=? (- (rank a) k r) f)
         a)
        ((not (array? a))
         (match-frame (make-array a) f r k))
        (else
         (apply make-shared-array a
                (lambda i (append (take i k) (take (drop i k) (- (rank a) k r)) (take-right i r)))
                (append (take ($ a) k) f (take-right ($ a) r))))))

(define (prefix-frame a r k)
  "Frame common to all arrays a with cell ranks r, ignoring first k axes."
  (fold (lambda (a r f)
          (let ((fa (drop-right! (drop ($ a) k) r)))
            (let loop ((s f) (sa fa))
              (cond ((null? sa) f)
                    ((null? s) fa)
                    ((= (car s) (car sa)) (loop (cdr s) (cdr sa)))
                    (else (error "shape clash" ($ a) r f k))))))
        '() a r))

(define (nested-op-frames op k . a)
  (let loop ((op op) (ff '()) (a a) (k 0))
    (let* ((r (apply verb-actual-ri op (map (lambda (a) (- (rank a) k)) a)))
           (f (prefix-frame a r k))
           (a (map! (cut match-frame <> f <> k) a r))
           (op (%verb-op op)))
      (if (procedure? op)
        (values (concatenate! (reverse! (cons f ff))) op a)
        (loop op (cons f ff) a (+ k (length f)))))))


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
