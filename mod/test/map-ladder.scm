; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2016-2019, 2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Working up to ra-slice-for-each.
;;; Code:

(define-module (test map-ladder)
  #:export (ra-slice-for-each-1 ra-slice-for-each-2 ra-slice-for-each-3 ra-slice-for-each-4))

(import (newra base) (srfi :26) (srfi :71) (ice-9 control)
        (only (rnrs base) vector-for-each)
        (only (newra map) ra-slice-for-each-check make-ra-root-prefix ra-slice-for-each))

(define ra-slice-for-each-4 ra-slice-for-each)

; ra-slice-for-each-1/2/3/4 (in newra map) do the same thing at increasing levels of inlining
; and complication, except that only ra-slice-for-each-4 supports prefix matching.
; these other versions are kept for testing.

; slice recursively.
(define (ra-slice-for-each-1 kk op . ra)
  (let ((los lens (apply ra-slice-for-each-check kk ra)))
    (let loop-rank ((k 0) (ra ra))
      (if (= k kk)
        (apply op ra)
        (let* ((lo (vector-ref los k))
               (end (+ lo (vector-ref lens k))))
          (let loop-dim ((i lo))
            (unless (= i end)
              (loop-rank (+ k 1) (map (cut ra-slice <> i) ra))
              (loop-dim (+ i 1)))))))))

; a single moving slice for each argument.
(define (ra-slice-for-each-2 kk op . frame)
  (let* ((los lens (apply ra-slice-for-each-check kk frame))
; create (rank(ra) - k) slices that we'll use to iterate by bumping their zeros.
         (ra (map (cut make-ra-root-prefix <> kk los) frame)))
    (let loop-rank ((k 0))
      (if (= k kk)
; no fresh slice descriptor like in array-slice-for-each. Should be all right b/c the descriptors can be copied.
        (apply op ra)
        (let  ((lenk (vector-ref lens k)))
          (let loop-dim ((i 0))
            (cond
             ((= i lenk)
              (for-each
                  (lambda (ra frame)
                    (let ((step (dim-step (vector-ref (%%ra-dims frame) k))))
                      (%%ra-zero-set! ra (- (%%ra-zero ra) (* step lenk)))))
                ra frame))
             (else
              (loop-rank (+ k 1))
              (for-each
                  (lambda (ra frame)
                    (let ((step (dim-step (vector-ref (%%ra-dims frame) k))))
                      (%%ra-zero-set! ra (+ (%%ra-zero ra) step))))
                ra frame)
              (loop-dim (+ i 1))))))))))

; moving slice with row-major unrolling.
(define (ra-slice-for-each-3 u op . frame)
  (let ((los lens (apply ra-slice-for-each-check u frame)))
    (let/ec exit
; check early so we can save a step in the loop later.
      (vector-for-each (lambda (len) (when (zero? len) (exit))) lens)
; create (rank(ra) - k) slices that we'll use to iterate by bumping their zeros.
      (let ((ra (map (cut make-ra-root-prefix <> u los) frame)))
; since we'll unroll, special case for rank 0
        (if (zero? u)
          (apply op ra)
; we'll do a rank-loop in [0..u) and unroll dimensions [u..k); u must be searched.
          (let* ((u (- u 1))
                 (step (map (lambda (frame) (%%ra-step frame u)) frame))
                 (u len (let loop ((u u) (s step) (len 1))
                          (let ((lenu (vector-ref lens u)))
                            (if (zero? u)
                              (values u (* len lenu))
                              (let ((ss (map (cut * lenu <>) s))
                                    (sm (map (lambda (frame) (%%ra-step frame (- u 1))) frame)))
                                (if (equal? ss sm)
                                  (loop (- u 1) ss (* len lenu))
                                  (values u (* len lenu))))))))
                 (lenm (- len 1)))
            (let loop-rank ((k 0))
              (if (= k u)
; unrolled dimensions.
                (let loop ((i lenm))
; no fresh slice descriptor like in array-slice-for-each. Should be all right b/c the descriptors can be copied.
                  (apply op ra)
                  (cond
                   ((zero? i)
                    (for-each (lambda (ra step)
                                (%%ra-zero-set! ra (- (%%ra-zero ra) (* step lenm))))
                      ra step))
                   (else
                    (for-each (lambda (ra step)
                                (%%ra-zero-set! ra (+ (%%ra-zero ra) step)))
                      ra step)
                    (loop (- i 1)))))
                (let ((lenmk (- (vector-ref lens k) 1)))
                  (let loop-dim ((i lenmk))
                    (loop-rank (+ k 1))
                    (cond
                     ((zero? i)
                      (for-each (lambda (ra frame)
                                  (%%ra-zero-set! ra (- (%%ra-zero ra) (* (%%ra-step frame k) lenmk))))
                        ra frame))
                     (else
                      (for-each (lambda (ra frame)
                                  (%%ra-zero-set! ra (+ (%%ra-zero ra) (%%ra-step frame k))))
                        ra frame)
                      (loop-dim (- i 1))))))))))))))
