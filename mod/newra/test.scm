; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2017
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Alternative implementations (both for ra and array) for testing and benchmarking.
;;; Code:

(define-module (newra test)
  #:export (ra-map*! array-map*! array-print* %ra-loop ra-loop array-loop))

(import (newra base) (newra map)
        (only (rnrs base) vector-map vector-for-each) (rnrs io ports) (srfi srfi-4 gnu))

(define ra-map*!
  (case-lambda
   ((ra-slice-for-each o f)
    (ra-slice-for-each
     (ra-rank o)
     (lambda (o) (ra-set! o (f)))
     o)
    o)
   ((ra-slice-for-each o f a0)
    (ra-slice-for-each
     (ra-rank o)
     (lambda (o a0) (ra-set! o (f (ra-ref a0))))
     o a0)
    o)
   ((ra-slice-for-each o f a0 a1)
    (ra-slice-for-each
     (ra-rank o)
     (lambda (o a0 a1) (ra-set! o (f (ra-ref a0) (ra-ref a1))))
     o a0 a1)
    o)
   ((ra-slice-for-each o f . args)
    (apply ra-slice-for-each (ra-rank o)
           (lambda (o . args) (ra-set! o (apply f (map ra-ref args))))
           o args)
    o)))

(define (array-map*! o f . args)
  (apply array-slice-for-each (array-rank o)
         (lambda (o . args)
           (array-set! o (apply f (map array-ref args))))
         o args)
  o)

(define array-print-prefix (@@ (ice-9 arrays) array-print-prefix))

; this is a direct translation of scm_i_print_array_dimension() in arrays.c.
(define (array-print* a port)
  (define lo caar)
  (define hi cadar)
  (array-print-prefix a port)
  (let ((s (array-shape a))
        (i (shared-array-increments a))
        (r (shared-array-root a))
        (b (shared-array-offset a)))
    (let ((ref (case (array-type r)
                 ((#t) vector-ref)
                 ((c64) c64vector-ref)
                 ((c32) c32vector-ref)
                 ((f64) f64vector-ref)
                 ((f32) f32vector-ref)
                 ((s64) s64vector-ref)
                 ((s32) s32vector-ref)
                 ((s16) s16vector-ref)
                 ((s8)  s8vector-ref)
                 ((u64) u64vector-ref)
                 ((u32) u32vector-ref)
                 ((u16) u16vector-ref)
                 ((u8) u8vector-ref)
                 ((a) string-ref)
                 ((b) bitvector-ref)
                 (else (throw 'bad-type (array-type r))))))
; special case
      (if (zero? (array-rank a))
        (begin
          (display #\( port)
          (display (ref b) port)
          (display #\) port))
        (let loop ((ss s) (ii i) (b b))
          (if (null? ss)
            (display (ref r b) port)
            (let ((lo (lo ss))
                  (hi (hi ss))
                  (i (car ii)))
              (put-char port #\()
              (do ((j lo (+ 1 j))
                   (b b (+ b i)))
                  ((> j hi))
                (loop (cdr ss) (cdr ii) b)
                (when (< j hi)
                  (put-char port #\space)))
              (put-char port #\)))))))))

(define-syntax %ra-loop-inner
  (lambda (stx-inner)
    (syntax-case stx-inner ()
      ((_ lens k u body nn ...)
       (let ((uu (syntax->datum #'u)))
         (if (= uu (syntax->datum #'k))
           #'body
           (with-syntax ((nu (list-ref #'(nn ...) uu)))
             #`(let ((end (vector-ref lens u)))
                 (let loop ((nu 0))
                   (unless (= nu end)
                     (%ra-loop-inner lens k #,(+ uu 1) body nn ...)
                     (loop (+ nu 1))))))))))))

(define-syntax %ra-loop
  (lambda (stx)
    (syntax-case stx ()
      ((_ lens k (i ...) body)
       #'(begin
           (unless (= (vector-length lens) k) (throw 'bad-rank))
           (%ra-loop-inner lens k 0 body i ...))))))

(define (loop-fun dims f)
  (case (vector-length dims)
    ((0) (%ra-loop dims 0 () (f)))
    ((1) (%ra-loop dims 1 (i) (f i)))
    ((2) (%ra-loop dims 2 (i j) (f i j)))
    ((3) (%ra-loop dims 3 (i j k) (f i j k)))
    ((4) (%ra-loop dims 4 (i j k l) (f i j k l)))
    ((5) (%ra-loop dims 5 (i j k l m) (f i j k l m)))
    (else (throw 'not-implemented))))

(define (ra-loop ra f)
  (loop-fun (vector-map dim-len (ra-dims ra)) f))

(define (array-loop a f)
  (loop-fun (list->vector (array-dimensions a)) f))
