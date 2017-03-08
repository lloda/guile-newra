
; Replacement for Guile C-based arrays - Printing
; (c) Daniel Llorens - 2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Printer for ra objects. They start with #% instead of #, otherwise the syntax
;; is the same as for regular Guile arrays.
;;; Code:

(define-module (newra print)
  #:export (ra-print-prefix ra-print))

(import (rnrs io ports) (rnrs base) (srfi srfi-1) (srfi srfi-4 gnu)
        (newra newra) (srfi srfi-26))

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
       (unless (zero? lo)
         (display #\@ port)
         (display (dim-lo dim) port)))
     (when dims?
       (display #\: port)
       (display (dim-len dim) port)))
   (ra-dims ra)))

(define* (ra-print ra port #:key (dims? #t))
  (ra-print-prefix ra port #:dims? dims?)
  (let ((base (ra-pos-first (ra-zero ra) (ra-dims ra)))
        (ref (cute (ra-vref ra) (ra-data ra) <>))
        (rank (ra-rank ra)))
; special case
    (if (zero? rank)
      (begin
        (display #\( port)
        (display (ref base) port)
        (display #\) port))
      (let loop ((k 0) (b base))
        (let* ((dim (vector-ref (ra-dims ra) k))
               (lo (dim-lo dim))
               (hi (+ lo (dim-len dim) -1))
               (i (dim-step dim)))
          (display #\( port)
          (cond
           ((= (- rank 1) k)
            (do ((j lo (+ 1 j)) (b b (+ b i)))
                ((> j hi))
              (display (ref b) port)
              (when (< j hi)
                (display #\space port))))
           (else
            (do ((j lo (+ 1 j)) (b b (+ b i)))
                ((> j hi))
              (loop (+ k 1) b)
              (when (< j hi)
                (display #\space port)))))
          (display #\) port))))))

(struct-set! (@@ (newra newra) <ra-vtable>) vtable-index-printer ra-print)
