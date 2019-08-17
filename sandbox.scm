; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2016-2017
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Trying things.

(import (newra newra) (newra tools) (newra base) (rnrs io ports)
        (srfi :8) (srfi :26) (ice-9 match) (srfi :1) (ice-9 format)
        (only (rnrs base) vector-map))

(define vector-append (@@ (newra lib) vector-append))
(vector-append #(1 2 3) #(x 5 6))


; -----------------------
; from
; -----------------------

(define A (ra-map! (make-ra 0 10 10) + (ra-transpose (ra-iota 10) 1) (ra-iota 10 0 10)))
(define b (ra-i 2))
(define c (make-ra-root (make-dim #f 10 2) (vector (make-dim 3 1))))
(define d (ra-i 2 2))

; -----------------------
; this is the pure beaten section.
; FIXME preallocate the dim vector then run j k ... j ...
; -----------------------

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
; this is the pure unbeaten section
; ------------------------

(define (fromu A . ai)
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
      (if #f ;; (= frame (ra-rank A))
; optimization
        (apply ra-map! B A i)
        (apply ra-slice-for-each frame
               (lambda (B . i) (ra-copy! B (apply (lambda i (apply ra-slice A i)) (map ra-ref i))))
               B i))
      B)))

(define (beatable? x)
  (or (and (ra? x) (or (zero? (ra-rank x)) (dim? (ra-root x)))) (integer? x)))
(define (index-rank x)
  (match x ((? integer? z) 0) ((? ra? ra) (ra-rank ra))))

(define from
  (case-lambda
   ((A . i)
    (let loop ((n 0) (m 0) (ii i)
               (ib '()) (ibi '()) (tb '())
               (iu '()) (iui '()) (tu '()))
      (match ii
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
; beat them
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
             (apply ra-transpose B (append tu tb)))))
        ((i0 . irest)
         (let* ((k (index-rank i0))
                (idest (iota k m)))
           (if (beatable? i0)
             (loop (+ n 1) (+ m k) irest
                   (cons i0 ib) (cons n ibi) (fold cons tb idest)
                   iu iui tu)
             (loop (+ n 1) (+ m k) irest
                   ib ibi tb
                   (cons i0 iu) (cons n iui) (fold cons tu idest))))))))))

(define iz (ra-i 3))
(from A iz)
(from A 0)
(from A (ra-copy #t (ra-iota 3)))
(from A (ra-iota 3) (ra-iota 2)) ; BUG


; ------------------------
; tests
; ------------------------

(define (test-from-from? A . i)
; beatable vs unbeatable
  (unless (ra-equal? (apply fromb A i) (apply fromu A i))
    (throw 'error-A i))
; general as unbeatable (except integers), vs unbeatable
  (unless (ra-equal? (apply from A (map (lambda (x) (if (ra? x) (ra-copy #t x) x)) i))
                     (apply fromu A i))
    (throw 'error-C))
; general as beatable, vs unbeatable
  (unless (ra-equal? (apply from A i) (apply fromu A i))
    (throw 'error-B))
; general as unbeatable/beatable (2 args), vs unbeatable
  (match i
    ((i j)
     (unless (ra-equal? (from A (if (ra? i) (ra-copy #t i) i) j)
                        (fromu A i j))
       (throw 'error-D))
     (unless (ra-equal? (from A i (if (ra? j) (ra-copy #t j) j))
                        (fromu A i j))
       (throw 'error-E)))
    (else #f)))

; ------------------------
; one arg

; rank 0
(test-from-from? A (make-ra-root (make-dim #f 3) (vector)))
(test-from-from? A 2)
(test-from-from? A (make-ra 2))

; rank 1
(test-from-from? A (ra-iota 3))
(test-from-from? A (make-ra-root (make-dim #f) (vector (make-dim 3 1 1))))
(test-from-from? A (make-ra-root (make-dim 3 1 1) (vector (make-dim 3 1 1))))
(test-from-from? A (make-ra-root (make-dim 3 1 2) (vector (make-dim 3 1 1))))
(test-from-from? A (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2))))
(test-from-from? A (make-ra-root (make-dim #f 3 2) (vector (make-dim 2 1 3))))

; rank 2
(test-from-from? A (ra-i 2 2))
(test-from-from? A (make-ra-root (make-dim #f 3) (vector (make-dim 2 1 2) (make-dim 2 1 3))))


; ------------------------
; two args

; rank 0 0
(test-from-from? A (make-ra-root (make-dim #f 3) (vector))
                 (make-ra-root (make-dim #f 2) (vector)))
(test-from-from? A 3 (make-ra-root (make-dim #f 2) (vector)))
(test-from-from? A (make-ra-root (make-dim #f 3) (vector)) 2)
(test-from-from? A 3 2)

; rank 1 1
(test-from-from? A (ra-iota 3) (ra-iota 2 4))
(test-from-from? A (make-ra-root (make-dim #f) (vector (make-dim 3 1 1)))
                 (make-ra-root (make-dim #f) (vector (make-dim 3 1 2))))
(test-from-from? A (make-ra-root (make-dim 3 1 1) (vector (make-dim 3 1 1)))
                 (make-ra-root (make-dim 3 1 2) (vector (make-dim 3 1 1))))
(test-from-from? A (make-ra-root (make-dim 3 1 2) (vector (make-dim 3 1 1)))
                 (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2))))
(test-from-from? A (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2)))
                 (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2))))
(test-from-from? A (make-ra-root (make-dim #f 3 2) (vector (make-dim 2 1 3)))
                 (make-ra-root (make-dim 6 1 1) (vector (make-dim 3 1 2))))

; rank 2 2
(test-from-from? A (ra-i 3 3) (ra-i 2 2))
(test-from-from? A (make-ra-root (make-dim #f 3) (vector (make-dim 2 1 2) (make-dim 2 1 3)))
                 (make-ra-root (make-dim #f 3) (vector (make-dim 2 1 2) (make-dim 2 1 3))))

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
