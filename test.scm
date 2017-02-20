
; Replacement for Guile C-based array system - Tests
; (c) Daniel Llorens - 2016-2017
; Run with $GUILE -L mod -s test.scm

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(import (srfi srfi-64)
        (newra newra) (newra print) (newra tools)
        (only (rnrs base) vector-map)
        (srfi srfi-26) (srfi srfi-8) (only (srfi srfi-1) fold)
        (ice-9 match))

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
; array->ra, ra->array, printers
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

(test-equal  #(1 2 3)            (ra->array ra0))
(test-equal  #@1(1 2 3)          (ra->array ra1))
(test-equal  #2((1 2) (3 4))     (ra->array ra2))
(test-equal  #2@1@1((1 2) (3 4)) (ra->array ra3))
(test-equal  #0(99)              (ra->array ra4))

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

; applicable!
(test-equal (call-with-output-string (cut display (ra8) <>)) "%2d@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (call-with-output-string (cut display (ra8 1) <>)) "%1d@1:3(1 2 3)")
(test-equal (call-with-output-string (cut display (ra8 2) <>)) "%1d@1:3(4 5 6)")
(test-equal 5 (ra8 2 2))

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

; -----------------------
; ra-slice-for-each
; -----------------------

(for-each
  (lambda (ra-slice-for-each)
    (test-begin (procedure-name ra-slice-for-each))

    (test-equal "%2@1:2@1:3((1 2 3) (4 5 6))\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 0 (lambda (o) (format s"~a\n" o)) ra6))))
    (test-equal "%1@1:3(1 2 3)\n%1@1:3(4 5 6)\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 1 (lambda (o) (format s "~a\n" o)) ra6))))
    (test-equal "1\n2\n3\n4\n5\n6\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 2 (lambda (o) (format s "~a\n" (ra-ref o))) ra6))))
    (test-equal "2\n4\n6\n8\n10\n12\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 2 (lambda (a b) (format s "~a\n" (+ (ra-ref a) (ra-ref b)))) ra6 ra7))))

    (test-end (procedure-name ra-slice-for-each)))
  (list ra-slice-for-each-1 ra-slice-for-each-2))

; -----------------------
; setter
; -----------------------

(define ra9 (make-ra-data (make-vector 6) '(-1 0) '(1 3)))
(set! (ra9 -1 1) 99)
(set! (ra9 -1 2) 77)
(set! (ra9 -1 3) 88)
(set! (ra9 0 1) 33)
(set! (ra9 0 2) 11)
(set! (ra9 0 3) 22)
(test-equal (call-with-output-string (cut display ra9 <>)) "%2@-1:2@1:3((99 77 88) (33 11 22))")

; -----------------------
; we have enough for ra-map!
; -----------------------

(define ra10 (make-ra-new #t 0 '(0 10)))

(define (ra-map! ra-slice-for-each o f . args)
  (apply ra-slice-for-each (ra-rank o)
         (lambda (o . args) (ra-type o)
                 (ra-set! o (apply f (map ra-ref args))))
         o args)
  o)

(define (array-map*! o f . args)
  (apply array-slice-for-each (array-rank o)
         (lambda (o . args) (array-type o)
                 (array-set! o (apply f (map array-ref args))))
         o args)
  o)


(define ra11 (make-ra-new #t 0 10))
(define ra12 (make-ra-data (make-dim 10) 10))
(define ra13 (make-ra-data (make-dim 10 10 -1) 10))
(test-equal "%1:10(-10 -8 -6 -4 -2 0 2 4 6 8)"
            (call-with-output-string (cute display (ra-map! ra-slice-for-each ra11 - ra12 ra13) <>)))

(test-end "newra")
(unless (zero? (test-runner-fail-count (test-runner-current)))
  (error "FAILED test.scm"))

; -----------------------
; benchmark rank 1
; -----------------------

(define type #t)

(define n 50000)
(define ra20 (make-ra-new type *unspecified* n))
(define ra21 (ra-map! ra-slice-for-each (make-ra-new type 0 n) identity (make-ra-data (make-dim n) n)))
(define ra22 (ra-map! ra-slice-for-each (make-ra-new type 0 n) identity (make-ra-data (make-dim n n -1) n)))
(define a20 (ra->array ra20))
(define a21 (ra->array ra21))
(define a22 (ra->array ra22))

(format #t "\n~8,6f\n" (time (ra-map! ra-slice-for-each-1 ra20 - ra21 ra22)))
(format #t "~8,6f\n" (time (ra-map! ra-slice-for-each-2 ra20 - ra21 ra22)))
(format #t "~8,6f\n" (time (array-map*! a20 - a21 a22)))
(format #t "~8,6f\n" (time (array-map! a20 - a21 a22)))

; -----------------------
; benchmark rank 2
; -----------------------

(define n 200)
(define ra20 (make-ra-new type *unspecified* n n))
(define ra21 (ra-map! ra-slice-for-each (make-ra-new type 0 n n) (lambda () (random n))))
(define ra22 (ra-map! ra-slice-for-each (make-ra-new type 0 n n) (lambda () (random n))))
(define a20 (ra->array ra20))
(define a21 (ra->array ra21))
(define a22 (ra->array ra22))

(format #t "\n~8,6f\n" (time (ra-map! ra-slice-for-each-1 ra20 - ra21 ra22)))
(format #t "~8,6f\n" (time (ra-map! ra-slice-for-each-2 ra20 - ra21 ra22)))
(format #t "~8,6f\n" (time (array-map*! a20 - a21 a22)))
(format #t "~8,6f\n" (time (array-map! a20 - a21 a22)))

; -----------------------
; benchmark rank 3
; -----------------------

(define n 37)
(define ra20 (make-ra-new type *unspecified* n n n))
(define ra21 (ra-map! ra-slice-for-each (make-ra-new type 0 n n n) (lambda () (random n))))
(define ra22 (ra-map! ra-slice-for-each (make-ra-new type 0 n n n) (lambda () (random n))))
(define a20 (ra->array ra20))
(define a21 (ra->array ra21))
(define a22 (ra->array ra22))

(format #t "\n~8,6f\n" (time (ra-map! ra-slice-for-each-1 ra20 - ra21 ra22)))
(format #t "~8,6f\n" (time (ra-map! ra-slice-for-each-2 ra20 - ra21 ra22)))
(format #t "~8,6f\n" (time (array-map*! a20 - a21 a22)))
(format #t "~8,6f\n" (time (array-map! a20 - a21 a22)))

; -----------------------
; some profiling...
; -----------------------

(import (statprof))
(define prof (lambda () (ra-map! ra-slice-for-each-2 ra20 * ra21 ra22)))
(statprof prof #:count-calls? #t #:display-style 'tree)
