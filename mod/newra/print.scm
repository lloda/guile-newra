
; Replacement for Guile C-based arrays - Printing
; (c) Daniel Llorens - 2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(define-module (newra print)
  #:export (ra-print-prefix ra-print))
(import (rnrs io ports) (rnrs base) (srfi srfi-1) (srfi srfi-4 gnu)
        (newra newra))

; FIXME still need to extend (truncated-print).

(define* (ra-print-prefix a port #:key dims?)
  (display #\% port)
  (display (ra-rank a) port)
  (let ((type (ra-type a)))
    (unless (eq? #t type)
      (display type port)))
  (vector-for-each
   (lambda (dim)
     (let ((lo (dim-lo dim)))
       (unless (zero? lo)
         (display #\@ port)
         (display (dim-lo dim) port)))
     (display #\: port)
     (display (dim-len dim) port))
   (ra-dims a)))

(define* (ra-print a port #:key dims?)
  (ra-print-prefix a port #:dims? dims?)
; first position on the ra.
  (let ((b (ra-pos-first (ra-zero a) (ra-dims a)))
        (ref (let ((vref (ra-vref a))
                   (data (ra-data a)))
               (lambda (i) (vref data i)))))
; special case
    (if (zero? (ra-rank a))
      (begin
        (display #\( port)
        (display (ref b) port)
        (display #\) port))
      (let loop ((p 0) (b b))
        (if (= (ra-rank a) p)
          (display (ref b) port)
          (let* ((dim (vector-ref (ra-dims a) p))
                 (lo (dim-lo dim))
                 (hi (+ lo (dim-len dim) -1))
                 (i (dim-step dim)))
            (display #\( port)
            (do ((j lo (+ 1 j))
                 (b b (+ b i)))
                ((> j hi))
              (loop (+ p 1) b)
              (when (< j hi)
                (display #\space port)))
            (display #\) port)))))))

(struct-set! (@@ (newra newra) <ra-vtable>) vtable-index-printer ra-print)
