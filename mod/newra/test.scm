
; (c) Daniel Llorens - 2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Alternative implementations for testing and benchmarking.
;;; Code:

(define-module (newra test)
  #:export (ra-map*! array-map*!))

(import (newra newra) (only (rnrs base) vector-map vector-for-each))

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
