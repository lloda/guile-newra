
; Replacement for Guile C-based array system - Tests
; (c) Daniel Llorens - 2016-2017
; Run with $GUILE -L mod -s test.scm

(import (newra newra) (newra print) (newra tools)
        (only (rnrs base) vector-map) (only (srfi srfi-1) fold)
        (srfi srfi-26) (ice-9 match)
        (srfi srfi-64))

(define (throws-exception? k thunk)
  (catch #t (lambda () (thunk) #f)
         (lambda (kk . args)
           (if (eq? k kk)
             (cons kk args)
             #f))))

(set! test-log-to-file #f)
(test-begin "newra")

; -----------------------
; auxiliary functions
; -----------------------

(test-equal
 6 ((@@ (newra newra) vector-fold) (lambda (a c) (+ c (car a))) 0 #((1) (2) (3))))

(test-equal
 #(2 3) ((@@ (newra newra) vector-clip) #(1 2 3 4) 1 3))

; -----------------------
; array->ra & printers
; -----------------------

(define ra0 (array->ra #(1 2 3)))
(define ra1 (array->ra #@1(1 2 3)))
(define ra2 (array->ra #2((1 2) (3 4))))
(define ra3 (array->ra #2@1@1((1 2) (3 4))))
(define ra4 (array->ra #0(99)))

(test-equal (call-with-output-string (cut display ra0 <>)) "%1:3(1 2 3)")
(test-equal (call-with-output-string (cut display ra1 <>)) "%1@1:3(1 2 3)")
(test-equal (call-with-output-string (cut display ra2 <>)) "%2:2:2((1 2) (3 4))")
(test-equal (call-with-output-string (cut display ra3 <>)) "%2@1:2@1:2((1 2) (3 4))")
(test-equal (call-with-output-string (cut display ra4 <>)) "%0(99)")

; -----------------------
; make-ra-new
; -----------------------

(define ra5 (make-ra-new #t 0 '(1 3) '(1 2)))
(array-index-map! (ra-data ra5) (lambda i i))
(test-equal (call-with-output-string (cut display ra5 <>)) "%2@1:3@1:2(((0) (1)) ((2) (3)) ((4) (5)))")

; -----------------------
; make-ra-data
; -----------------------

(define ra6 (make-ra-data #(1 2 3 4 5 6) '(1 2) '(1 3)))
(test-equal (call-with-output-string (cut display ra6 <>)) "%2@1:2@1:3((1 2 3) (4 5 6))")

(define ra7 (make-ra-data (make-dim 6 1) '(1 2) '(1 3)))
(test-equal (call-with-output-string (cut display ra7 <>)) "%2d@1:2@1:3((1 2 3) (4 5 6))")

; -----------------------
; ra-slice, ra-ref, ra-cell
; -----------------------

(define ra8 (make-ra-data (make-dim 6 1) '(1 2) '(1 3)))

(test-equal (call-with-output-string (cut display (ra-cell ra8) <>)) "%2d@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (call-with-output-string (cut display (ra-cell ra8 1) <>)) "%1d@1:3(1 2 3)")
(test-equal (call-with-output-string (cut display (ra-cell ra8 2) <>)) "%1d@1:3(4 5 6)")
(test-equal 5 (ra-cell ra8 2 2))

(test-equal (call-with-output-string (cut display (ra-slice ra8) <>)) "%2d@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (call-with-output-string (cut display (ra-slice ra8 1) <>)) "%1d@1:3(1 2 3)")
(test-equal (call-with-output-string (cut display (ra-slice ra8 2) <>)) "%1d@1:3(4 5 6)")
(test-equal (call-with-output-string (cut display (ra-slice ra8 2 2) <>)) "%0d(5)")
(test-equal 5 (ra-ref (ra-slice ra8 2 2)))

(test-assert (throws-exception? 'bad-number-of-indices (lambda () (ra-ref ra8))))
(test-assert (throws-exception? 'bad-number-of-indices (lambda () (ra-ref ra8 1))))
(test-assert (throws-exception? 'dim-check-out-of-range (lambda () (ra-ref ra8 0 0))))
(test-assert (throws-exception? 'dim-check-out-of-range (lambda () (ra-ref ra8 3 1))))
(test-equal 1 (ra-ref ra8 1 1))
(test-equal 2 (ra-ref ra8 1 2))
(test-equal 3 (ra-ref ra8 1 3))
(test-equal 4 (ra-ref ra8 2 1))
(test-equal 5 (ra-ref ra8 2 2))
(test-equal 6 (ra-ref ra8 2 3))

;; (define ra0 (make-ra (make-vector 10 3) 0 (vector (make-dim 3))))
;; (define ra1 (make-ra (make-vector 10 3) 0 (vector (make-dim 3) (make-dim 3))))

;; (ra-print-prefix ra0 (current-output-port))
;; (ra-print ra1 (current-output-port))


;; ;; ; ways to go
;; ;; (define array0 (make-typed-array 'f64 9 10 10 10 10 10 2))
;; ;; (define t0 (time (call-with-output-file "/dev/null" (cut display array0 <>))))
;; ;; (define t1 (time (call-with-output-file "/dev/null" (cut ra-print (array->ra array0) <>))))
;; ;; (define t2 (time (call-with-output-file "/dev/null" (cut (@@ (ice-9 arrays) array-print) array0 <>))))

;; ; now array-cell, etc.

;; (define ra0 (make-ra-c (make-dim 10) 10))
;; (define ra2 (make-ra (make-dim 10) 0 (vector (make-dim 10))))

(test-end "newra")
(unless (zero? (test-runner-fail-count (test-runner-current)))
  (error "FAILED test.scm"))
