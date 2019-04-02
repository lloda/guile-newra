
; (c) Daniel Llorens - 2019

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Laziness and composition for newra.
;;; Code:

(define-module (newra expr))

(import (newra newra) (only (srfi srfi-1) fold every) (srfi srfi-71) (srfi srfi-26) (ice-9 match))


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

(define (make-verb op . ranks)
  (make-struct/no-tail
             <verb-vtable>
             (lambda i (throw 'you-tried-to-call op ranks i))
             op ranks '()))

(define (make-verb-alternates op-ranks0 . op-ranks)
  (match (cons op-ranks0 op-ranks)
    (((op0 ranks0) (op ranks) ...)
     (unless (let check ((ranks0 ranks0) (ranks ranks))
               (or (null? ranks)
                   (and (= (length ranks0) (length (car ranks)))
                        (every <= ranks0 (car ranks))
                        (check (car ranks) (cdr ranks)))))
       (throw 'bad-alternates))
; TODO maybe we want to store the alternates differently? probably want to match them last to first.
     (make-struct/no-tail
      <verb-vtable>
      (lambda i (throw 'you-tried-to-call-alternates op0 ranks0 i))
      op0 ranks0 op-ranks))))

(define +!. (make-verb (lambda (a b c) (ra-set! c (+ (ra-ref a) (ra-ref b)))) 0 0 0))
(define +!. (make-verb-alternates (list (lambda (a b c) (ra-set! c (+ (ra-ref a) (ra-ref b)))) (list 0 0 0))))


; ----------------
; 1.b application (rank extension) of verbs w/o return values.
; ----------------





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
