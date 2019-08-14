; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2016-2017
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Trying things.

(import (newra newra) (newra tools) (newra base) (rnrs io ports)
        (srfi :8) (srfi :26) (ice-9 match) (srfi :1)
        (only (rnrs base) vector-map))

(define (vector-append . a)
  (let ((b (make-vector (fold (lambda (a c) (+ (vector-length a) c)) 0 a))))
    (let loopa ((a a) (lo 0))
      (if (null? a)
        b
        (let ((lena (vector-length (car a))))
          (do ((j 0 (+ j 1)))
              ((= j lena))
            (vector-set! b (+ lo j) (vector-ref (car a) j)))
          (loopa (cdr a) (+ lo lena)))))))

(vector-append #(1 2 3) #(x 5 6))


; -----------------------
; from
; -----------------------

(define A (ra-map! (make-ra 0 10 10) + (ra-transpose (ra-iota 10) 1) (ra-iota 10 0 10)))
(define b (ra-i 2))
(define c (make-ra-root (make-dim #f 10 2) (vector (make-dim 3 1))))
(define d (ra-i 2 2))

; this is the pure beaten section.
; FIXME preallocate the dim vector then run j k ... j ...

(define (fromb A . ai)
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
                  bdims)))))))


; ------------------------
; this is the pure beaten section WIP
; ------------------------

(define (fromu A . ai)
  (let* ((bdims (apply c-dims (append-map (compose ra-shape check-ra) ai)))
         (B (make-ra-new (ra-type A) *unspecified* bdims)))
    B))


; ------------------------
; one arg

; rank 0
(fromb A (pk 'I (make-ra-root (make-dim #f 3) (vector))))
(fromb A 2)
(fromb A (make-ra 2))

; rank 1
(fromb A (pk 'I (ra-iota 3)))
(fromb A (pk 'I (make-ra-root (make-dim #f) (vector (make-dim 3 1 1)))))
(fromb A (pk 'I (make-ra-root (make-dim 3 1 1) (vector (make-dim 3 1 1)))))
(fromb A (pk 'I (make-ra-root (make-dim 3 1 2) (vector (make-dim 3 1 1)))))
(fromb A (pk 'I (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2)))))
(fromb A (pk 'I (make-ra-root (make-dim #f 3 2) (vector (make-dim 2 1 3)))))

; rank 2
(fromb A (pk 'I (ra-i 2 2)))
(fromb A (pk 'I (make-ra-root (make-dim #f 3) (vector (make-dim 2 1 2) (make-dim 2 1 3)))))


; ------------------------
; two args

; rank 0 0
(fromb A (pk 'I (make-ra-root (make-dim #f 3) (vector)))
       (pk 'J (make-ra-root (make-dim #f 2) (vector))))
(fromb A 3 (pk 'J (make-ra-root (make-dim #f 2) (vector))))
(fromb A (pk 'I (make-ra-root (make-dim #f 3) (vector))) 2)
(fromb A 3 2)

; rank 1 1
(fromb A (pk 'I (ra-iota 3)) (pk 'J (ra-iota 2 4)))
(fromb A (pk 'I (make-ra-root (make-dim #f) (vector (make-dim 3 1 1))))
       (pk 'J (make-ra-root (make-dim #f) (vector (make-dim 3 1 2)))))
(fromb A (pk 'I (make-ra-root (make-dim 3 1 1) (vector (make-dim 3 1 1))))
       (pk 'J (make-ra-root (make-dim 3 1 2) (vector (make-dim 3 1 1)))))
(fromb A (pk 'I (make-ra-root (make-dim 3 1 2) (vector (make-dim 3 1 1))))
       (pk 'J (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2)))))
(fromb A (pk 'I (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2))))
       (pk 'J (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2)))))
(fromb A (pk 'I (make-ra-root (make-dim #f 3 2) (vector (make-dim 2 1 3))))
       (pk 'J (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2)))))

; rank 2 2
(fromb A (pk 'I (ra-i 3 3))
       (pk 'J (ra-i 2 2)))
(fromb A (pk 'I (make-ra-root (make-dim #f 3) (vector (make-dim 2 1 2) (make-dim 2 1 3))))
       (pk 'J (make-ra-root (make-dim #f 3) (vector (make-dim 2 1 2) (make-dim 2 1 3)))))


; ------------------------
; this is the pure beaten section WIP

(define (fromu A . ai)
  (let* ((ai (map (match-lambda ((? ra? x) x) ((? integer? x) (make-ra x))) ai))
         (bshape
          (pk 'BSHAPE (append
                       (append-map ra-shape ai)
                       (map (lambda (dim) (list (dim-lo dim) (dim-hi dim))) (drop (vector->list (ra-dims A)) (length ai))))))
         (bdims (pk 'BDIMS (apply c-dims bshape)))
         (bstairs (pk 'STAIRS (reverse (cdr (fold (lambda (a c) (cons (+ (ra-rank a) (car c)) c)) '(0) ai)))))
         (B (make-ra-new (ra-type A) *unspecified* bdims)))
;FIXME must match lo on dead axes.
;FIXME handle the rest axes (drop ... (length ai)) above.
    (apply ra-map! B A
           (map (lambda (ai stairs)
                  (pk 'STAIRED (apply ra-transpose ai (iota (ra-rank ai) stairs))))
             ai bstairs))))


; ------------------------
; two args

; rank 0 0
(fromu A (pk 'I (make-ra-root (make-dim #f 3) (vector)))
       (pk 'J (make-ra-root (make-dim #f 2) (vector))))
(fromu A 3 (pk 'J (make-ra-root (make-dim #f 2) (vector))))
(fromu A (pk 'I (make-ra-root (make-dim #f 3) (vector))) 2)
(fromu A 3 2)

(throw 'stop)

; rank 1 1
(fromu A (pk 'I (ra-iota 3)) (pk 'J (ra-iota 2 4)))
(fromu A (pk 'I (make-ra-root (make-dim #f) (vector (make-dim 3 1 1))))
       (pk 'J (make-ra-root (make-dim #f) (vector (make-dim 3 1 2)))))
(fromb A (pk 'I (make-ra-root (make-dim 3 1 1) (vector (make-dim 3 1 1))))
       (pk 'J (make-ra-root (make-dim 3 1 2) (vector (make-dim 3 1 1)))))
(fromb A (pk 'I (make-ra-root (make-dim 3 1 2) (vector (make-dim 3 1 1))))
       (pk 'J (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2)))))
(fromb A (pk 'I (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2))))
       (pk 'J (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2)))))
(fromb A (pk 'I (make-ra-root (make-dim #f 3 2) (vector (make-dim 2 1 3))))
       (pk 'J (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2)))))

; rank 2 2
(fromb A (pk 'I (ra-i 3 3))
       (pk 'J (ra-i 2 2)))
(fromb A (pk 'I (make-ra-root (make-dim #f 3) (vector (make-dim 2 1 2) (make-dim 2 1 3))))
       (pk 'J (make-ra-root (make-dim #f 3) (vector (make-dim 2 1 2) (make-dim 2 1 3)))))


; ------------------------
; one arg

; rankp 0
(fromu A (pk 'I (make-ra-root (make-dim #f 3) (vector))))
(fromu A 2)
(fromu A (make-ra 2))

; rank 1
(fromb A (pk 'I (ra-iota 3)))
(fromu A (pk 'I (make-ra-root (make-dim #f) (vector (make-dim 3 1 1)))))
(fromu A (pk 'I (make-ra-root (make-dim 3 1 1) (vector (make-dim 3 1 1)))))
(fromu A (pk 'I (make-ra-root (make-dim 3 1 2) (vector (make-dim 3 1 1)))))
(fromu A (pk 'I (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2)))))
(fromu A (pk 'I (make-ra-root (make-dim #f 3 2) (vector (make-dim 2 1 3)))))

; rank 2
(fromb A (pk 'I (ra-i 2 2)))
(fromb A (pk 'I (make-ra-root (make-dim #f 3) (vector (make-dim 2 1 2) (make-dim 2 1 3)))))

(throw 'stop)

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

;; (import (newra newra) (newra tools) (rnrs io ports)
;;         (srfi :8) (srfi :26) (ice-9 match) (only (srfi :1) fold)
;;         (only (rnrs base) vector-map))
