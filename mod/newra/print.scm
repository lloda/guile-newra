; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2017-2018
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Printer for ra objects. They start with #% instead of #, otherwise the syntax
;; is the same as for regular Guile arrays. Loading this module installs the
;; printer.
;;; Code:

(define-module (newra print)
  #:export (ra-print-prefix ra-print))

(import (rnrs io ports) (rnrs base) (srfi :1) (srfi :4 gnu) (srfi :26)
        (ice-9 match) (newra base) (newra map))

; FIXME still need to extend (truncated-print).

(define* (ra-print-prefix ra port #:key (dims? #t))
  (display #\# port)
  (display #\% port)
  (display (ra-rank ra) port)
  (let ((type (ra-type ra)))
    (unless (eq? #t type)
      (display type port)))
  (vector-for-each
   (lambda (dim)
     (let ((lo (dim-lo dim)))
       (unless (or (not lo) (zero? lo))
         (display #\@ port)
         (display (or lo 'f) port)))
     (when dims?
       (display #\: port)
       (display (match (dim-len dim)
; print len of dead axes with 'd and of infinite axes with 'f.
                  (#f (if (zero? (dim-step dim)) 'd  'f))
                  (len len))
                port)))
   (ra-dims ra)))

(define* (ra-print ra port #:key (dims? #t))
  (ra-print-prefix ra port #:dims? dims?)
  (let ((base (ra-offset (ra-zero ra) (ra-dims ra)))
        (ref (cute (ra-vref ra) (ra-root ra) <>))
        (rank (ra-rank ra)))
; special case
    (if (zero? rank)
      (begin
        (display #\( port)
        (write (ref base) port)
        (display #\) port))
      (let loop ((k 0) (b base))
        (let* ((dim (vector-ref (ra-dims ra) k))
               (i (dim-step dim))
               (lo (dim-lo dim))
; print dead axes as if of size 1. Infinite arrays aren't printed (FIXME?)
               (len (or (dim-len dim) (if (zero? i) 1 #f))))
          (when len
            (let ((hi (+ (or lo 0) len -1)))
              (display #\( port)
              (cond
               ((= (- rank 1) k)
                (do ((j (or lo 0) (+ 1 j)) (b b (+ b i)))
                    ((> j hi))
                  (write (ref b) port)
                  (when (< j hi)
                    (display #\space port))))
               (else
                (do ((j (or lo 0) (+ 1 j)) (b b (+ b i)))
                    ((> j hi))
                  (loop (+ k 1) b)
                  (when (< j hi)
                    (display #\space port)))))
              (display #\) port))))))))

(struct-set! (@ (newra base) <ra-vtable>) vtable-index-printer ra-print)
