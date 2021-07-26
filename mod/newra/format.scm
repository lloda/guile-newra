; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Library for Newra - Pretty printer for arrays
;;; Code:

(define-module (newra format)
  #:export (ra-format))

(import (newra base) (newra map) (newra cat)  (newra from) (newra lib)
        (srfi :1) (srfi :26) (srfi :71) (ice-9 match) (ice-9 format))

(define* (sc-print sc #:optional (o #t))
  (let ((o (match o
             (#t (current-output-port))
             (#f (throw 'bad-output-spec))
             (o o))))
    (ra-slice-for-each 1
      (lambda (line)
        (ra-for-each (cut display <> o) line)
        (newline o))
      sc)))

(define art-0 "│─┌┐└┘├┤┬┴┼")
(define art-1 "║═╔╗╚╝╠╣╦╩╬")
(define art-2 "┃━┏┓┗┛┣┫┳┻╋")
(define art-3 "████████████")
(define art-x "-|+++++++++")
(define arts (make-ra-root (vector art-0 art-1 art-2 art-3)))

(define* (ra-format port ra #:optional (fmt "~a"))
; size the cells
  (define s (ra-map! (apply make-ra #f (ra-dimensions ra))
                     (lambda (x)
                       (if (ra? x)
                         (ra-format #f x fmt)
                         (ra-tile (array->ra (format #f fmt x)) 1)))
                     ra))
  (define-values (dim0 dim1)
    (let* ((q r (euclidean/ (ra-rank s) 2))
           (a (ra-iota (+ q r) 0 2))
           (b (ra-iota q 1 2)))
      (if (zero? r)
        (values a b)
        (values b a))))
  (define (lengths dim0 dim1 k)
    (let* ((sq (apply ra-untranspose s (ra->list (ra-pcat #f 0 dim1 dim0))))
           (l (apply make-ra 0 (drop (ra-dimensions sq) (ra-len dim1))))
           (border (if (zero? (ra-rank l)) 0 1)))
      (ra-slice-for-each-in-order (ra-len dim1)
        (lambda (w)
          (ra-map! l (lambda (l w) (max l (+ border (ra-len w k)))) l w))
        sq)
      l))
  (define l0 (lengths dim0 dim1 0))
  (define l1 (lengths dim1 dim0 1))
  (define t0 (- (ra-fold + 0 l0) (if (zero? (ra-rank l0)) 1 0)))
  (define t1 (- (ra-fold + 0 l1) (if (zero? (ra-rank l1)) 1 0)))
  (define (line-0 sc k range at) (ra-amend! sc (string-ref (ra-ref arts k) 0) range at))
  (define (line-1 sc k range at) (ra-amend! sc (string-ref (ra-ref arts k) 1) at range))
; define positions for grid and cells
  (define (scan-0 ra)
    (let ((c (ra-copy ra))
          (s 0))
      (ra-map! c (lambda (c) (let ((d s)) (set! s (+ s c)) d)) c)))
  (define (scan-1 b)
    (let* ((c (make-ra 0 (+ 1 (ra-len b))))
           (cv (ra-from c (ra-iota (ra-len b) 1)))
           (s 0))
      (ra-map! cv (lambda (c) (let ((d s)) (set! s (+ s c)) d) s) b)
      c))
  (define (marks l k)
    (and (>= k 0)
         (let ((m (apply make-ra 0 (take (ra-dimensions l) (+ k 1)))))
           (ra-slice-for-each (+ k 1) (lambda (l m) (set! (m) (ra-fold + 0 l)) m) l m)
           (scan-1 (ra-ravel m)))))
; make screen and print
  (define sc (make-typed-ra 'a #\space (+ 1 t0) (+ 1 t1)))
  (if (zero? (ra-rank ra))
    (ra-copy! sc (s))
    (begin
; grid
      (let loop ((k 0))
        (let* ((m0 (marks l0 (- (ra-rank l0) 1 k)))
               (m1 (marks l1 (- (ra-rank l1) 1 k)))
               (>m0< (and m0 (ra-from m0 (ra-iota (- (ra-len m0) 2) 1))))
               (>m1< (and m1 (ra-from m1 (ra-iota (- (ra-len m1) 2) 1)))))
          (cond ((and m0 m1)
                 (ra-for-each (lambda (m0) (line-1 sc k (ra-iota (+ 1 t1) 0) m0)) m0)
                 (ra-for-each (lambda (m1) (line-0 sc k (ra-iota (+ 1 t0) 0) m1)) m1)
                 (ra-for-each (lambda (m0 m1) (ra-set! sc (string-ref (ra-ref arts k) 10) m0 m1))
                              >m0< (ra-transpose >m1< 1))
                 (ra-for-each (lambda (m1)
                                (ra-set! sc (string-ref (ra-ref arts k) 8) 0 m1)
                                (ra-set! sc (string-ref (ra-ref arts k) 9) t0 m1))
                              >m1<)
                 (ra-for-each (lambda (m0)
                                (ra-set! sc (string-ref (ra-ref arts k) 6) m0 0)
                                (ra-set! sc (string-ref (ra-ref arts k) 7) m0 t1))
                              >m0<)
                 (ra-set! sc (string-ref (ra-ref arts k) 2) 0 0)
                 (ra-set! sc (string-ref (ra-ref arts k) 3) 0 t1)
                 (ra-set! sc (string-ref (ra-ref arts k) 4) t0 0)
                 (ra-set! sc (string-ref (ra-ref arts k) 5) t0 t1)
                 (loop (+ k 1)))
                (m1
                 (ra-for-each (lambda (m1) (line-0 sc k (ra-iota (+ t0 1) 0) m1)) m1))
                (else #f))))
; cells
      (ra-for-each
       (lambda (sq o0 l0 o1 l1)
         (ra-copy! (ra-from sc
                            (ra-iota (ra-len sq 0) (+ o0 (if (> (ra-rank s) 1) 1 0)))
                            (ra-iota (ra-len sq 1) (+ o1 1 (- l1 (ra-len sq 1) 1)))) ; align right
                   sq)
         )
       (apply ra-untranspose s (ra->list (ra-pcat #f 0 dim0 dim1)))
       (apply ra-reshape (scan-0 (ra-ravel l0)) (ra-dimensions l0))
       l0
       (ra-transpose (apply ra-reshape (scan-0 (ra-ravel l1)) (ra-dimensions l1)) (ra-rank l0))
       (ra-transpose l1 (ra-rank l0)))))
  (if port
    (sc-print sc port)
    sc))
