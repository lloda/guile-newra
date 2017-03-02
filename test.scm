
; Replacement for Guile C-based array system - Tests
; (c) Daniel Llorens - 2016-2017
; Run with $GUILE -L mod -s test.scm

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(import (srfi srfi-64)
        (newra newra) (newra test) (newra print) (newra tools) (newra read)
        (only (rnrs base) vector-map)
        (srfi srfi-26) (srfi srfi-8) (only (srfi srfi-1) fold iota)
        (ice-9 match))

(define (throws-exception? k thunk)
  (catch #t (lambda () (thunk) #f)
         (lambda (kk . args)
           (if (eq? k kk)
             (cons kk args)
             #f))))

(define (ra->string ra) (call-with-output-string (cut display ra <>)))

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

(test-equal (ra->string ra0) "%1:3(1 2 3)")
(test-equal (ra->string ra1) "%1@1:3(1 2 3)")
(test-equal (ra->string ra2) "%2:2:2((1 2) (3 4))")
(test-equal (ra->string ra3) "%2@1:2@1:2((1 2) (3 4))")
(test-equal (ra->string ra4) "%0(99)")

(test-equal  #(1 2 3)            (ra->array ra0))
(test-equal  #@1(1 2 3)          (ra->array ra1))
(test-equal  #2((1 2) (3 4))     (ra->array ra2))
(test-equal  #2@1@1((1 2) (3 4)) (ra->array ra3))
(test-equal  #0(99)              (ra->array ra4))


; -----------------------
; make-shared-ra
; -----------------------

(test-equal "%0(99)" (ra->string (make-shared-ra ra4 (lambda () '()))))

(test-equal "%1:2(1 4)" (ra->string (make-shared-ra ra2 (lambda (i) (list i i)) 2)))
(test-equal "%1:2(1 3)" (ra->string (make-shared-ra ra2 (lambda (i) (list i 0)) 2)))
(test-equal "%1:2(2 4)" (ra->string (make-shared-ra ra2 (lambda (i) (list i 1)) 2)))
(test-equal "%1:2(1 2)" (ra->string (make-shared-ra ra2 (lambda (j) (list 0 j)) 2)))
(test-equal "%1:2(3 4)" (ra->string (make-shared-ra ra2 (lambda (j) (list 1 j)) 2)))

(test-equal "%1@1:2(1 4)" (ra->string (make-shared-ra ra3 (lambda (i) (list i i)) '(1 2))))
(test-equal "%1@1:2(1 3)" (ra->string (make-shared-ra ra3 (lambda (i) (list i 1)) '(1 2))))
(test-equal "%1@1:2(2 4)" (ra->string (make-shared-ra ra3 (lambda (i) (list i 2)) '(1 2))))
(test-equal "%1@1:2(1 2)" (ra->string (make-shared-ra ra3 (lambda (j) (list 1 j)) '(1 2))))
(test-equal "%1@1:2(3 4)" (ra->string (make-shared-ra ra3 (lambda (j) (list 2 j)) '(1 2))))


; -----------------------
; make-ra-new
; -----------------------

(define ra5 (make-ra-new #t 0 '(1 3) '(1 2)))
(array-index-map! (ra-data ra5) (lambda i i))
(test-equal (ra->string ra5) "%2@1:3@1:2(((0) (1)) ((2) (3)) ((4) (5)))")
(test-equal 3 (ra-length ra5))


; -----------------------
; make-ra-data
; -----------------------

(define ra6 (make-ra-data #(1 2 3 4 5 6) '(1 2) '(1 3)))
(test-equal (ra->string ra6) "%2@1:2@1:3((1 2 3) (4 5 6))")
(define ra7a (make-ra-data (make-dim 6 1) '(1 2) '(1 3)))
(test-equal (ra->string ra7a) "%2d@1:2@1:3((1 2 3) (4 5 6))")
(define ra7b (make-ra #(1 4 2 5 3 6) -3 `#(,(make-dim 2 1 1) ,(make-dim 3 1 2))))
(test-equal (ra->string ra7b) "%2@1:2@1:3((1 2 3) (4 5 6))")

(test-equal 2 (ra-length ra6))
(test-equal 2 (ra-length ra7a))
(test-equal 2 (ra-length ra7b))


; -----------------------
; ra-slice, ra-ref, ra-cell
; -----------------------

(define ra8 (make-ra-data (make-dim 6 1) '(1 2) '(1 3)))
(test-equal 2 (ra-length ra8))

(test-equal (ra->string (ra-cell ra8)) "%2d@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra-cell ra8 1)) "%1d@1:3(1 2 3)")
(test-equal (ra->string (ra-cell ra8 2)) "%1d@1:3(4 5 6)")
(test-equal 5 (ra-cell ra8 2 2))

; applicable!
(test-equal (ra->string (ra8)) "%2d@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra8 1)) "%1d@1:3(1 2 3)")
(test-equal (ra->string (ra8 2)) "%1d@1:3(4 5 6)")
(test-equal 5 (ra8 2 2))

(test-equal (ra->string (ra-slice ra8)) "%2d@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra-slice ra8 1)) "%1d@1:3(1 2 3)")
(test-equal (ra->string (ra-slice ra8 2)) "%1d@1:3(4 5 6)")
(test-equal (ra->string (ra-slice ra8 2 2)) "%0d(5)")
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
; ra-transpose
; -----------------------

(test-equal (ra->string (ra-transpose ra7a #(1 0))) "%2d@1:3@1:2((1 4) (2 5) (3 6))")
(test-equal (ra->string (ra-transpose ra7b #(1 0))) "%2@1:3@1:2((1 4) (2 5) (3 6))")
(test-equal (ra->string (ra-transpose ra7a #(0 1))) "%2d@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra-transpose ra7b #(0 1))) "%2@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra-transpose ra7a #(0 0))) "%1d@1:2(1 5)")
(test-equal (ra->string (ra-transpose ra7b #(0 0))) "%1@1:2(1 5)")


; -----------------------
; ra-slice-for-each
; -----------------------

(define ra-empty0 (make-ra-data (make-dim 6 1) '(1 0) '(2 1)))
(define ra-empty1 (make-ra-data (make-dim 6 1) '(1 1) '(2 1)))
(define ra-empty2 (make-ra-data (make-dim 6 1) '(1 0) '(2 2)))
(test-equal 0 (ra-length ra-empty0))
(test-equal 1 (ra-length ra-empty1))
(test-equal 0 (ra-length ra-empty2))

(for-each
  (lambda (ra-slice-for-each)
    (test-begin (procedure-name ra-slice-for-each))

    (test-equal "%2d@1:0@2:0()\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 0 (lambda (o) (format s "~a\n" o)) ra-empty0))))
    (test-equal "%2d@1:1@2:0(())\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 0 (lambda (o) (format s "~a\n" o)) ra-empty1))))
    (test-equal "%2d@1:0@2:1()\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 0 (lambda (o) (format s "~a\n" o)) ra-empty2))))

    (test-equal "%2@1:2@1:3((1 2 3) (4 5 6))\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 0 (lambda (o) (format s "~a\n" o)) ra6))))
    (test-equal "%1@1:3(1 2 3)\n%1@1:3(4 5 6)\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 1 (lambda (o) (format s "~a\n" o)) ra6))))
    (test-equal "1\n2\n3\n4\n5\n6\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 2 (lambda (o) (format s "~a\n" (ra-ref o))) ra6))))

    (test-equal "2\n4\n6\n8\n10\n12\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 2 (lambda (a b) (format s "~a\n" (+ (ra-ref a) (ra-ref b)))) ra6 ra7a))))
    (test-equal "2\n4\n6\n8\n10\n12\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 2 (lambda (a b) (format s "~a\n" (+ (ra-ref a) (ra-ref b)))) ra6 ra7b))))

    (test-equal "(%2@1:2@1:3((1 2 3) (4 5 6)) %2d@1:2@1:3((1 2 3) (4 5 6)) %2@1:2@1:3((1 2 3) (4 5 6)))\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 0 (lambda x (format s "~a\n" x)) ra6 ra7a ra7b))))
    (test-equal "(%1@1:3(1 2 3) %1d@1:3(1 2 3) %1@1:3(1 2 3))\n(%1@1:3(4 5 6) %1d@1:3(4 5 6) %1@1:3(4 5 6))\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 1 (lambda x (format s "~a\n" x)) ra6 ra7a ra7b))))
    (test-equal "(%0(1) %0d(1) %0(1))\n(%0(2) %0d(2) %0(2))\n(%0(3) %0d(3) %0(3))\n(%0(4) %0d(4) %0(4))\n(%0(5) %0d(5) %0(5))\n(%0(6) %0d(6) %0(6))\n"
                (call-with-output-string
                 (lambda (s) (ra-slice-for-each 2 (lambda x (format s "~a\n" x)) ra6 ra7a ra7b))))

    (test-end (procedure-name ra-slice-for-each)))
  (list ra-slice-for-each-1 ra-slice-for-each-2
        ra-slice-for-each-3 ra-slice-for-each-4))


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
(test-equal (ra->string ra9) "%2@-1:2@1:3((99 77 88) (33 11 22))")
(test-equal 2 (ra-length ra9))


; -----------------------
; test through the ra-map! interface, also ra-copy!, ra-fill!, ra-equal?
; -----------------------

(define ra11 (make-ra-new #t 0 10))
(define ra12 (make-ra-data (make-dim 10) 10))
(define ra13 (make-ra-data (make-dim 10 10 -1) 10))

(test-equal 10 (ra-length ra11))
(test-equal 10 (ra-length ra12))
(test-equal 10 (ra-length ra13))

(test-equal "%1:10(0 1 2 3 4 5 6 7 8 9)" (ra->string (ra-copy! ra12 ra11)))
(let ((ra5a (make-ra-new #t 0 '(1 2) '(1 3)))
      (ra6a (make-ra-data (vector-copy #(1 2 3 4 5 6)) '(1 2) '(1 3))))
  (test-equal "%2@1:2@1:3((1 2 3) (4 5 6))" (ra->string (ra-copy! ra6a ra5a)))
  (test-assert (ra-equal? ra6a ra5a))
  (set! (ra5a 1 1) 99)
  (test-assert (not (ra-equal? ra5a ra6a)))
  (set! (ra6a 1 1) 99)
  (test-assert (ra-equal? ra5a ra6a))
  (test-equal "%2@1:2@1:3((x x x) (x x x))" (ra->string (ra-fill! ra5a 'x)))
  (test-equal "%2@1:2@1:3((x x x) (x x x))" (ra->string ra5a))
  (test-assert (not (ra-equal? ra6a ra5a))))

(test-begin "ra-for-each")
(test-equal "(10 0 10)(9 1 9)(8 2 8)(7 3 7)(6 4 6)(5 5 5)(4 6 4)(3 7 3)(2 8 2)(1 9 1)"
            (call-with-output-string (lambda (s) (ra-for-each (lambda x (display x s)) ra13 ra12 ra13))))
(test-end "ra-for-each")

(ra-map! ra11 (lambda () (random 100)))

(for-each
  (match-lambda
    ((ra-map! name)
      (test-begin (string-append "3 args - " name))
      (test-equal "%1:10(-10 -8 -6 -4 -2 0 2 4 6 8)"
                  (call-with-output-string (cute display (ra-map! ra11 - ra12 ra13) <>)))
      (test-end (string-append "3 args - " name))))
  `((,(cut ra-map*! ra-slice-for-each-1  <...>) "fe1")
    (,(cut ra-map*! ra-slice-for-each-2  <...>) "fe2")
    (,(cut ra-map*! ra-slice-for-each-3  <...>) "fe3")
    (,(cut ra-map*! ra-slice-for-each-4  <...>) "fe4")
    (,ra-map! "ra-map!")))

; test with enough args to hit the arglist version.
(for-each
  (match-lambda
    ((ra-map! name)
      (test-begin (string-append "4 args - " name))
      (test-equal "%1:10(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1)"
                  (call-with-output-string (cute display (ra-map! ra11 - ra12 ra13 ra12) <>)))
      (test-end (string-append "4 args - " name))))
  `((,(cut ra-map*! ra-slice-for-each-1  <...>) "fe1")
    (,(cut ra-map*! ra-slice-for-each-2  <...>) "fe2")
    (,(cut ra-map*! ra-slice-for-each-3  <...>) "fe3")
    (,(cut ra-map*! ra-slice-for-each-4  <...>) "fe4")
    (,ra-map! "ra-map!")))

(test-begin "ra-for-each")
(test-equal "(10 0 10)(9 1 9)(8 2 8)(7 3 7)(6 4 6)(5 5 5)(4 6 4)(3 7 3)(2 8 2)(1 9 1)"
            (call-with-output-string (lambda (s) (ra-for-each (lambda x (display x s)) ra13 ra12 ra13))))
(test-end "ra-for-each")


; -----------------------
; reader
; -----------------------

(test-equal "%3:2:2:2(((1 2) (3 4)) ((5 6) (7 8)))"
            (ra->string (call-with-input-string "#%3(((1 2) (3 4)) ((5 6) (7 8)))" read)))
(test-equal "%2:2:2((1 2) (3 4))"
            (ra->string (call-with-input-string "#%2((1 2) (3 4))" read)))
(test-equal "%1:4(1 2 3 4)"
            (ra->string (call-with-input-string "#%1(1 2 3 4)" read)))
(test-equal "%1:12(1 2 3 4 5 6 7 8 9 10 11 12)"
            (ra->string (call-with-input-string "#%1(1 2 3 4 5 6 7 8 9 10 11 12)" read)))
(test-equal "%2@1:2@1:2((1 2) (3 4))"
            (ra->string (call-with-input-string "#%2@1@1((1 2) (3 4))" read)))
(test-equal "%2@1:2@1:2((1 2) (3 4))"
            (ra->string (call-with-input-string "#%2@1@1:2((1 2) (3 4))" read)))
(test-equal "%2@-1:2@1:2((1 2) (3 4))"
            (ra->string (call-with-input-string "#%2@-1@1((1 2) (3 4))" read)))
(test-equal "%0(#(1 2 3))"
            (ra->string (call-with-input-string "#%0(#(1 2 3))" read)))


; -----------------------
; the end.
; -----------------------

(test-end "newra")
(unless (zero? (test-runner-fail-count (test-runner-current)))
  (error "FAILED test.scm"))
