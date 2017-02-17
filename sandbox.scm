
; Replacement for Guile C-based array system - WIP
; (c) Daniel Llorens - 2016-2017

(import (newra newra) (newra print) (newra tools)
        (only (rnrs base) vector-map) (only (srfi srfi-1) fold)
        (srfi srfi-26) (ice-9 match))

(define ra0 (make-ra (make-vector 10 3) 0 (vector (make-dim 3))))
(define ra1 (make-ra (make-vector 10 3) 0 (vector (make-dim 3) (make-dim 3))))

(ra-print-prefix ra0 (current-output-port))
(ra-print ra1 (current-output-port))

(define ra0 (array->ra #(1 2 3)))
(define ra1 (array->ra #@1(1 2 3)))
(define ra2 (array->ra #2((1 2) (3 4))))
(define ra3 (array->ra #2@1@1((1 2) (3 4))))

;; ; ways to go
;; (define array0 (make-typed-array 'f64 9 10 10 10 10 10 2))
;; (define t0 (time (call-with-output-file "/dev/null" (cut display array0 <>))))
;; (define t1 (time (call-with-output-file "/dev/null" (cut ra-print (array->ra array0) <>))))
;; (define t2 (time (call-with-output-file "/dev/null" (cut (@@ (ice-9 arrays) array-print) array0 <>))))

(make-ra-c #t 0 2 3)
(make-ra-c #t 0 3 2)
(make-ra-c #t 0 '(1 3) 2)
(define ra4 (make-ra-c #t 0 '(1 3) '(1 2)))
(array-index-map! (ra-data ra4) (lambda i i))

; now array-cell, etc.

(define ra0 (make-ra-c (make-dim 10) 10))
(define ra2 (make-ra (make-dim 10) 0 (vector (make-dim 10))))
