; -*- mode: scheme; coding: utf-8 -*-
; guile-newra - Tests

; (c) Daniel Llorens - 2016-2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Run with $GUILE -L mod test/everything.scm

(import (newra) (newra tools) (newra read) (test misc) (test map-ladder)
        (srfi srfi-64) (srfi srfi-26) (srfi srfi-8)
        (only (srfi srfi-1) fold iota drop)
        (ice-9 format) (ice-9 match) (only (rnrs base) vector-map vector-for-each))

(define (ra->string ra) (call-with-output-string (cut display ra <>)))
(define (string->ra s) (call-with-input-string s read))

(test-begin "newra")


; -----------------------
; auxiliary functions
; -----------------------

(test-equal
  6 ((@ (newra vector) vector-fold) (lambda (a c) (+ c (car a))) 0 #((1) (2) (3))))

(test-equal
  #(2 3) (vector-copy #(1 2 3 4) 1 3)) ; in Guile 3.0.8

; loop-fun from (test misc) FIXME may become ra-index-map!

(define ra0 (make-ra-root (make-aseq) (c-dims)))
(define ra1 (make-ra-root (make-aseq) (c-dims 2)))
(define ra2 (make-ra-root (make-aseq) (c-dims 2 3)))
(define ra3 (make-ra-root (make-aseq) (c-dims 2 3 4)))

(test-assert (ra-equal? (make-ra-root #(1 2 3)) (list->ra 1 '(1 2 3))))
(test-assert (not (ra-equal? (make-ra 0 3 4) (make-ra 0 3)))) ; ra-equal? doesn't rank-extend

(test-equal "0"
  (with-output-to-string (lambda () (%ra-loop (vector-map dim-len (ra-dims ra0)) 0 () (display (ra0))))))
(test-equal "01"
  (with-output-to-string (lambda () (%ra-loop (vector-map dim-len (ra-dims ra1)) 1 (i) (display (+ i))))))
(test-equal "012123"
  (with-output-to-string (lambda () (%ra-loop (vector-map dim-len (ra-dims ra2)) 2 (i j) (display (+ i j))))))
(test-equal "012312342345123423453456"
  (with-output-to-string (lambda () (%ra-loop (vector-map dim-len (ra-dims ra3)) 3 (i j k) (display (+ i j k))))))

(define a0 (ra->array (as-ra ra0 #:type #t)))
(define a1 (ra->array (as-ra ra1 #:type #t)))
(define a2 (ra->array (as-ra ra2 #:type #t)))
(define a3 (ra->array (as-ra ra3 #:type #t)))

(test-equal "(0)"
  (call-with-output-string (lambda (o) (ra-loop ra0 (lambda () (format o "(~a)" (ra0)))))))
(test-equal "(0)(1)"
  (call-with-output-string (lambda (o) (ra-loop ra1 (lambda (i) (format o "(~a)" (ra1 i)))))))
(test-equal "(0)(1)(2)(3)(4)(5)"
  (call-with-output-string (lambda (o) (ra-loop ra2 (lambda (i j) (format o "(~a)" (ra2 i j)))))))
(test-equal "(0)(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11)(12)(13)(14)(15)(16)(17)(18)(19)(20)(21)(22)(23)"
  (call-with-output-string (lambda (o) (ra-loop ra3 (lambda (i j k) (format o "(~a)" (ra3 i j k)))))))

(test-equal "(0)"
  (call-with-output-string (lambda (o) (array-loop a0 (lambda () (format o "(~a)" (array-ref a0)))))))
(test-equal "(0)(1)"
  (call-with-output-string (lambda (o) (array-loop a1 (lambda (i) (format o "(~a)" (array-ref a1 i)))))))
(test-equal "(0)(1)(2)(3)(4)(5)"
  (call-with-output-string (lambda (o) (array-loop a2 (lambda (i j) (format o "(~a)" (array-ref a2 i j)))))))
(test-equal "(0)(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)(11)(12)(13)(14)(15)(16)(17)(18)(19)(20)(21)(22)(23)"
  (call-with-output-string (lambda (o) (array-loop a3 (lambda (i j k) (format o "(~a)" (array-ref a3 i j k)))))))


; -----------------------
; array->ra, ra->array, printers
; -----------------------

(let* ((ra0 (array->ra #(1 2 3)))
       (ra1 (array->ra #@1(1 2 3)))
       (ra2 (array->ra #2((1 2) (3 4))))
       (ra3 (array->ra #2@1@1((1 2) (3 4))))
       (ra4 (array->ra #0(99)))
       (ra5 (array->ra #0s64(99)))
       (ra6 (array->ra #0((a)))))

  (test-equal (ra->string ra0) "#%1:3(1 2 3)")
  (test-equal (ra->string ra1) "#%1@1:3(1 2 3)")
  (test-equal (ra->string ra2) "#%2:2:2((1 2) (3 4))")
  (test-equal (ra->string ra3) "#%2@1:2@1:2((1 2) (3 4))")
  (parameterize ((*ra-parenthesized-rank-zero* #t))
    (test-equal (ra->string ra4) "#%0(99)")
    (test-equal (ra->string ra5) "#%0s64(99)")
    (test-equal (ra->string ra6) "#%0((a))"))
  (parameterize ((*ra-parenthesized-rank-zero* #f))
    (test-equal (ra->string ra4) "#%0 99")
    (test-equal (ra->string ra5) "#%0s64 99")
    (test-equal (ra->string ra6) "#%0 (a)"))

  (test-equal  #(1 2 3)            (ra->array ra0))
  (test-equal  #@1(1 2 3)          (ra->array ra1))
  (test-equal  #2((1 2) (3 4))     (ra->array ra2))
  (test-equal  #2@1@1((1 2) (3 4)) (ra->array ra3))
  (test-equal  #0(99)              (ra->array ra4))
  (test-equal  #0s64(99)           (ra->array ra5))
  (test-equal  #0((a))             (ra->array ra6)))

; dead axes
(test-equal "#%2d:d:10((0 1 2 3 4 5 6 7 8 9))" (ra->string (ra-transpose (ra-i 10) 1)))

; printing of string elements (for example).
(test-equal "#%0(\"hello\")" (ra->string (make-ra "hello")))
(test-equal "#%1:2(\"hello\" \"bye\")" (ra->string (make-ra-root #("hello" "bye") (c-dims 2))))


; -----------------------
; ra-iota, ra-i
; -----------------------

(test-equal "#%3d:2:3:4(((0 1 2 3) (4 5 6 7) (8 9 10 11)) ((12 13 14 15) (16 17 18 19) (20 21 22 23)))"
  (ra->string (ra-i 2 3 4)))

(test-equal "#%3d:2:0:4(() ())"
  (ra->string (ra-i 2 0 4)))

(test-equal "#%0d(0)"
  (ra->string (ra-i)))

(test-equal "#%1d:4(9 8 7 6)"
  (ra->string (ra-iota 4 9 -1)))

(test-equal "#%1d:4(9 10 11 12)"
  (ra->string (ra-iota 4 9)))

(test-equal "#%1d:0()"
  (ra->string (ra-iota 0)))

; default ra-iota has lo #f so it will match any lower bound and act as index placeholder.
(test-equal "#%1@2:3(2 3 4)"
  (ra->string (ra-copy! (make-ra-new #t 'o (c-dims '(2 4))) (ra-iota))))

; mixing dead and unbounded axes
(let ((a (ra-i #f #t #f 2 3)))
  (test-equal 5 (a 0 0 0 1 2))
  (test-equal 5 (a 0 0 1 1 2))
  (test-equal 11 (a 0 1 0 1 2))
; reducing the unbounded axis
  (test-equal "#%4d:d:d:2:3((((6 7 8) (9 10 11))))"
    (ra->string (ra-from a #t 1))))

; unbounded both ways
(let ((a (ra-i #f '(#f #t) #f 2 3)))
  (test-equal "#%4d:d:d:2:3((((-6 -5 -4) (-3 -2 -1))))"
    (ra->string (ra-from a #t -1 #t))))

; 2/3/4 use moving slice but 1 uses direct slicing in the lo..hi range.

(define (test-lof rsfe)
  (test-equal
    "(#%0(o) #%0d(2))(#%0(o) #%0d(3))(#%0(o) #%0d(4))"
    (call-with-output-string
     (lambda (o)
       (rsfe 1 (lambda a (display a o)) (make-ra-new #t 'o (c-dims '(2 4))) (ra-iota))))))

(test-lof ra-slice-for-each-1)
(test-lof ra-slice-for-each-2)
(test-lof ra-slice-for-each-3)
(test-lof ra-slice-for-each-4)


; -----------------------
; make-ra-shared
; -----------------------

(let* ((ra2 (array->ra #2((1 2) (3 4))))
       (ra3 (array->ra #2@1@1((1 2) (3 4))))
       (ra4 (array->ra #0(99))))

  (test-equal "#%0(99)" (ra->string (make-ra-shared ra4 (lambda () '()))))

  (test-equal "#%1:2(1 4)" (ra->string (make-ra-shared ra2 (lambda (i) (list i i)) 2)))
  (test-equal "#%1:2(1 3)" (ra->string (make-ra-shared ra2 (lambda (i) (list i 0)) 2)))
  (test-equal "#%1:2(2 4)" (ra->string (make-ra-shared ra2 (lambda (i) (list i 1)) 2)))
  (test-equal "#%1:2(1 2)" (ra->string (make-ra-shared ra2 (lambda (j) (list 0 j)) 2)))
  (test-equal "#%1:2(3 4)" (ra->string (make-ra-shared ra2 (lambda (j) (list 1 j)) 2)))

  (test-equal "#%1@1:2(1 4)" (ra->string (make-ra-shared ra3 (lambda (i) (list i i)) '(1 2))))
  (test-equal "#%1@1:2(1 3)" (ra->string (make-ra-shared ra3 (lambda (i) (list i 1)) '(1 2))))
  (test-equal "#%1@1:2(2 4)" (ra->string (make-ra-shared ra3 (lambda (i) (list i 2)) '(1 2))))
  (test-equal "#%1@1:2(1 2)" (ra->string (make-ra-shared ra3 (lambda (j) (list 1 j)) '(1 2))))
  (test-equal "#%1@1:2(3 4)" (ra->string (make-ra-shared ra3 (lambda (j) (list 2 j)) '(1 2)))))


; -----------------------
; make-ra-new, make-ra
; -----------------------

(let* ((ra5 (make-ra-new #t 0 (c-dims '(1 3) '(1 2)))))
  (array-index-map! (ra-root ra5) (lambda i i))
  (test-equal (ra->string ra5) "#%2@1:3@1:2(((0) (1)) ((2) (3)) ((4) (5)))")
  (test-equal 3 (ra-len ra5)))

(let* ((ra5 (make-typed-ra 's64 0 '(1 3) '(1 2))))
  (array-index-map! (ra-root ra5) (lambda i (car i)))
  (test-equal (ra->string ra5) "#%2s64@1:3@1:2((0 1) (2 3) (4 5))")
  (test-equal 3 (ra-len ra5)))


; -----------------------
; make-ra-root
; -----------------------

(define ra6 (make-ra-root #(1 2 3 4 5 6) (c-dims '(1 2) '(1 3))))
(test-equal (ra->string ra6) "#%2@1:2@1:3((1 2 3) (4 5 6))")
(define ra7a (make-ra-root (make-aseq 1) (c-dims '(1 2) '(1 3))))
(test-equal (ra->string ra7a) "#%2d@1:2@1:3((1 2 3) (4 5 6))")
(define ra7b (make-ra-root #(1 4 2 5 3 6) `#(,(make-dim 2 1 1) ,(make-dim 3 1 2)) -3))
(test-equal (ra->string ra7b) "#%2@1:2@1:3((1 2 3) (4 5 6))")

(test-equal 2 (ra-len ra6))
(test-equal 2 (ra-len ra7a))
(test-equal 2 (ra-len ra7b))


; -----------------------
; ra-slice, ra-ref, ra-cell
; -----------------------

(define ra8 (make-ra-root (make-aseq 1) (c-dims '(1 2) '(1 3))))
(test-equal 2 (ra-len ra8))

(test-equal (ra->string (ra-cell ra8)) "#%2d@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra-cell ra8 1)) "#%1d@1:3(1 2 3)")
(test-equal (ra->string (ra-cell ra8 2)) "#%1d@1:3(4 5 6)")
(test-equal 5 (ra-cell ra8 2 2))

; applicable!
(test-equal (ra->string (ra8)) "#%2d@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra8 1)) "#%1d@1:3(1 2 3)")
(test-equal (ra->string (ra8 2)) "#%1d@1:3(4 5 6)")
(test-equal 5 (ra8 2 2))

(test-equal (ra->string (ra-slice ra8)) "#%2d@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra-slice ra8 1)) "#%1d@1:3(1 2 3)")
(test-equal (ra->string (ra-slice ra8 2)) "#%1d@1:3(4 5 6)")
(test-equal (ra->string (ra-slice ra8 2 2)) "#%0d(5)")
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
; ra-transpose (see below for ra-transpose with dead axes)
; -----------------------

(test-equal (ra->string (ra-transpose ra7a 1 0)) "#%2d@1:3@1:2((1 4) (2 5) (3 6))")
(test-equal (ra->string (ra-transpose ra7b 1 0)) "#%2@1:3@1:2((1 4) (2 5) (3 6))")
(test-equal (ra->string (ra-transpose ra7a 0 1)) "#%2d@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra-transpose ra7b 0 1)) "#%2@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra-transpose ra7a 0 0)) "#%1d@1:2(1 5)")
(test-equal (ra->string (ra-transpose ra7b 0 0)) "#%1@1:2(1 5)")
(test-assert (throws-exception? 'bad-number-of-axes (lambda () (ra-transpose (ra-iota 3) 0 1))))
(let ((ra0 (make-ra 99))) (test-assert (ra-equal? ra0 (ra-transpose ra0))))

; other cases

(test-equal (ra->string (ra-transpose ra7a)) "#%2d@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra-transpose ra7b)) "#%2@1:2@1:3((1 2 3) (4 5 6))")
(let ((ra1 (ra-i 3)))
  (test-assert (ra-equal? (ra-transpose ra1) (ra-transpose ra1 0)))
  (test-eq (ra-root ra1) (ra-root (ra-transpose ra1 0))))
(let ((ra2 (ra-i 3 4)))
  (test-assert (ra-equal? (ra-transpose ra2) (ra-transpose ra2 0 1)))
  (test-eq (ra-root ra2) (ra-root (ra-transpose ra2))))
(let ((ra3 (ra-i 3 4 5)))
  (test-assert (ra-equal? (ra-transpose ra3) (ra-transpose ra3 0 1 2)))
  (test-eq (ra-root ra3) (ra-root (ra-transpose ra3)))
  (test-assert (ra-equal? (ra-transpose ra3 1 0) (ra-transpose ra3 1 0 2)))
; these two have a dead axes so the cannot be matched one to the other.
  (test-equal (ra-dims (ra-transpose ra3 1)) (ra-dims (ra-transpose ra3 1 2 3))))

; untranspose

(let* ((a (ra-copy #t (ra-i 2 3 4)))
       (b (ra-transpose a 2 0 1))
       (c (ra-untranspose b 2 0 1)))
  (test-eq (ra-root a) (ra-root c))
  (test-assert (ra-equal? a c))
  (test-equal (ra-dims a) (ra-dims c)))

(let* ((a (ra-copy #t (ra-i 2 3 4 5)))
       (b (ra-transpose a 2 0 1))
       (c (ra-untranspose b 2 0 1)))
  (test-eq (ra-root a) (ra-root c))
  (test-assert (ra-equal? a c))
  (test-equal (ra-dims a) (ra-dims c)))


; -----------------------
; ra-reverse
; -----------------------

(let ((ra (ra-i 2 3 2)))
  (test-assert (ra-equal? ra (ra-reverse ra)))
  (test-equal "#%3d:2:3:2(((6 7) (8 9) (10 11)) ((0 1) (2 3) (4 5)))" (ra->string (ra-reverse ra 0)))
  (test-equal "#%3d:2:3:2(((4 5) (2 3) (0 1)) ((10 11) (8 9) (6 7)))" (ra->string (ra-reverse ra 1)))
  (test-equal "#%3d:2:3:2(((1 0) (3 2) (5 4)) ((7 6) (9 8) (11 10)))" (ra->string (ra-reverse ra 2)))
  (test-equal "#%3d:2:3:2(((5 4) (3 2) (1 0)) ((11 10) (9 8) (7 6)))" (ra->string (ra-reverse ra 1 2)))
  (test-equal "#%3d:2:3:2(((7 6) (9 8) (11 10)) ((1 0) (3 2) (5 4)))" (ra->string (ra-reverse ra 2 0)))
  (test-assert (ra-equal? (ra-reverse ra 2 0) (ra-reverse ra 0 2)))
  (test-assert (ra-equal? (ra-reverse ra 2 0 1) (ra-reverse (ra-reverse (ra-reverse ra 0) 1) 2)))
  (test-eq (ra-root ra) (ra-root (ra-reverse ra 2 0))))

(let ((ra (make-ra-root #(1 2 3 4) (vector (make-dim 4 -1)))))
  (test-equal "#%1@-1:4(4 3 2 1)" (ra->string (ra-reverse ra 0))))


; -----------------------
; ra-slice-for-each
; -----------------------

(define ra-empty0 (make-ra-root (make-aseq 1) (c-dims '(1 0) '(2 1))))
(define ra-empty1 (make-ra-root (make-aseq 1) (c-dims '(1 1) '(2 1))))
(define ra-empty2 (make-ra-root (make-aseq 1) (c-dims '(1 0) '(2 2))))
(test-equal 0 (ra-len ra-empty0))
(test-equal 1 (ra-len ra-empty1))
(test-equal 0 (ra-len ra-empty2))

(for-each
 (lambda (ra-slice-for-each)
   (test-begin (symbol->string (procedure-name ra-slice-for-each)))

   (test-equal "#%2d@1:0@2:0()\n"
     (call-with-output-string
      (lambda (s) (ra-slice-for-each 0 (lambda (o) (format s "~a\n" o)) ra-empty0))))
   (test-equal "#%2d@1:1@2:0(())\n"
     (call-with-output-string
      (lambda (s) (ra-slice-for-each 0 (lambda (o) (format s "~a\n" o)) ra-empty1))))
   (test-equal "#%2d@1:0@2:1()\n"
     (call-with-output-string
      (lambda (s) (ra-slice-for-each 0 (lambda (o) (format s "~a\n" o)) ra-empty2))))

   (test-equal "#%2@1:2@1:3((1 2 3) (4 5 6))\n"
     (call-with-output-string
      (lambda (s) (ra-slice-for-each 0 (lambda (o) (format s "~a\n" o)) ra6))))
   (test-equal "#%1@1:3(1 2 3)\n#%1@1:3(4 5 6)\n"
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

   (test-equal "(#%2@1:2@1:3((1 2 3) (4 5 6)) #%2d@1:2@1:3((1 2 3) (4 5 6)) #%2@1:2@1:3((1 2 3) (4 5 6)))\n"
     (call-with-output-string
      (lambda (s) (ra-slice-for-each 0 (lambda x (format s "~a\n" x)) ra6 ra7a ra7b))))
   (test-equal "(#%1@1:3(1 2 3) #%1d@1:3(1 2 3) #%1@1:3(1 2 3))\n(#%1@1:3(4 5 6) #%1d@1:3(4 5 6) #%1@1:3(4 5 6))\n"
     (call-with-output-string
      (lambda (s) (ra-slice-for-each 1 (lambda x (format s "~a\n" x)) ra6 ra7a ra7b))))
   (test-equal "(#%0(1) #%0d(1) #%0(1))\n(#%0(2) #%0d(2) #%0(2))\n(#%0(3) #%0d(3) #%0(3))\n(#%0(4) #%0d(4) #%0(4))\n(#%0(5) #%0d(5) #%0(5))\n(#%0(6) #%0d(6) #%0(6))\n"
     (call-with-output-string
      (lambda (s) (ra-slice-for-each 2 (lambda x (format s "~a\n" x)) ra6 ra7a ra7b))))

   (test-end (symbol->string (procedure-name ra-slice-for-each))))
 (list ra-slice-for-each-1 ra-slice-for-each-2
       ra-slice-for-each-3 ra-slice-for-each-4))


; -----------------------
; ra-offset - from guile/test-suite/tests/arrays.test
; -----------------------

(test-begin "ra-offset")

; plain vector
(test-equal 0 (ra-offset (make-ra 0 4)))
; plain array rank 2
(test-equal 0 (ra-offset (make-ra 0 4 4)))
; row of rank-2 array, I
(test-equal 0 (ra-offset ((make-ra 0 5 3) 0)))
; row of rank-2 array, II
(test-equal 4 (ra-offset ((make-ra 0 6 4) 1)))
; col of rank-2 array, I
(test-equal 0 (ra-offset ((ra-transpose (make-ra 0 5 3) 1 0) 0)))
; col of rank-2 array, II
(test-equal 1 (ra-offset ((ra-transpose (make-ra 0 6 4) 1 0) 1)))
; of inverted array; shared-array-root's doc is unclear as 'first' is unclear. But we do the same.
(test-equal 2 (shared-array-offset (make-shared-array #(1 2 3) (lambda (i) (list (- 2 i))) 3)))
(test-equal 2 (ra-offset (make-ra-shared (array->ra #(1 2 3)) (lambda (i) (list (- 2 i))) 3)))

(test-end "ra-offset")


; -----------------------
; ra-slice-for-each compatibility - from guile/test-suite/tests/array-map.test
; -----------------------

(for-each
    (match-lambda
      ((name ra-slice-for-each list->ra list->typed-ra make-ra ra-copy!
             make-ra-shared array->ra ra->array ra-set! ra-ref)
       (let ((test-name (format #f "~a" name)))
         (test-begin test-name)
; 1 argument frame rank 1
         (test-equal
           #2((1 3 9) (2 7 8))
           (ra->array
            (let* ((a (list->ra 2 '((9 1 3) (7 8 2)))))
              (ra-slice-for-each 1 (lambda (a) (sort! (ra->array a) <)) a)
              a)))
; 1 argument frame rank 1, non-zero base indices
         (test-equal
           #2@1@1((1 3 9) (2 7 8))
           (ra->array
            (let* ((a (make-ra *unspecified* '(1 2) '(1 3)))
                   (b (array->ra #2@1@1((9 1 3) (7 8 2)))))
              (ra-copy! a b)
              (ra-slice-for-each 1 (lambda (a) (sort! (ra->array a) <)) a)
              a)))
; 2 arguments frame rank 1
         (test-equal
           #f64(8 -1)
           (ra->array
            (let* ((x (list->typed-ra 'f64 2 '((9 1) (7 8))))
                   (y (array->ra (f64vector 99 99))))
              (ra-slice-for-each 1 (lambda (y x) (ra-set! y (- (ra-ref x 0) (ra-ref x 1)))) y x)
              y)))
; regression: zero-sized frame loop without unrolling
         (test-equal
           99
           (let* ((x 99)
                  (o (make-ra 0. 0 3 2)))
             (ra-slice-for-each 2
                                (lambda (o a0 a1)
                                  (set! x 0))
                                o
                                (make-ra-shared (make-ra 1. 0 1) (const '(0 0)) 0 3)
                                (make-ra 2. 0 3))
             x))
         (test-end test-name))))
  `(("oldra" ,array-slice-for-each ,list->array ,list->typed-array
     ,make-array ,(lambda (a b) (array-copy! b a)) ,make-shared-array
     ,(lambda (x) x) ,(lambda (x) x) ,array-set! ,array-ref)
    ("newra" ,ra-slice-for-each ,list->ra ,list->typed-ra
     ,make-ra ,ra-copy! ,make-ra-shared
     ,array->ra ,ra->array ,ra-set! ,ra-ref)
    ))


; -----------------------
; setter
; -----------------------

(define ra9 (make-ra-root (make-vector 6) (c-dims '(-1 0) '(1 3))))
(set! (ra9 -1 1) 99)
(set! (ra9 -1 2) 77)
(set! (ra9 -1 3) 88)
(set! (ra9 0 1) 33)
(set! (ra9 0 2) 11)
(set! (ra9 0 3) 22)
(test-equal (ra->string ra9) "#%2@-1:2@1:3((99 77 88) (33 11 22))")
(test-equal 2 (ra-len ra9))
(test-equal #2@-1:2@1:3((99 77 88) (33 44 22)) (ra->array (set! (ra9 0 2) 44)))


; -----------------------
; test through the ra-map! interface, also ra-copy!, ra-fill!, ra-swap! ra-equal?
; -----------------------

(define ra11 (make-ra-new #t 0 (c-dims 10)))
(define ra12 (make-ra-root (make-aseq) (c-dims 10)))
(define ra13 (make-ra-root (make-aseq 10 -1) (c-dims 10)))

(test-equal 10 (ra-len ra11))
(test-equal 10 (ra-len ra12))
(test-equal 10 (ra-len ra13))

(test-equal "#%1:10(0 1 2 3 4 5 6 7 8 9)" (ra->string (ra-copy! ra11 ra12)))
(test-equal "#%1:10(0 1 2 3 4 5 6 7 8 9)" (ra->string (ra-copy! ra11 (ra-copy #t ra12))))
(let ((ra5a (make-ra-new #t 0 (c-dims '(1 2) '(1 3))))
      (ra6a (make-ra-root (vector-copy #(1 2 3 4 5 6)) (c-dims '(1 2) '(1 3)))))
  (test-equal "#%2@1:2@1:3((1 2 3) (4 5 6))" (ra->string (ra-copy! ra5a ra6a)))
  (test-assert (ra-equal? ra6a ra5a))
  (test-assert (ra-equal? ra6a ra5a ra6a))
  (set! (ra5a 1 1) 99)
  (test-assert (not (ra-equal? ra5a ra6a)))
  (set! (ra6a 1 1) 99)
  (test-assert (ra-equal? ra5a ra6a))
  (test-equal "#%2@1:2@1:3((x x x) (x x x))" (ra->string (ra-fill! ra5a 'x)))
  (test-equal "#%2@1:2@1:3((x x x) (x x x))" (ra->string ra5a))
  (test-assert (not (ra-equal? ra6a ra5a)))
  (test-assert (not (ra-equal? (string->ra "#%(1 2 3)") (string->ra "#%(1 2 3 4)"))))
  (test-assert (not (ra-equal? (string->ra "#%1f64(1 2 3)") (string->ra "#%(1 2 3)"))))
  (test-assert (ra-equal? (make-ra-root #(99 99 99 99) (c-dims '(1 2) '(1 2)))
                          (ra-fill! (make-ra-root (vector 1 2 3 4) (c-dims '(1 2) '(1 2))) 99))))

(test-begin "ra-for-each")
(test-equal "(10 0 10)(9 1 9)(8 2 8)(7 3 7)(6 4 6)(5 5 5)(4 6 4)(3 7 3)(2 8 2)(1 9 1)"
  (call-with-output-string (lambda (s) (ra-for-each (lambda x (display x s)) ra13 ra12 ra13))))
(test-end "ra-for-each")

(ra-map! ra11 (lambda () (random 100)))

(for-each
 (match-lambda
   ((ra-map! name)
    (test-begin (string-append "3 args - " name))
    (test-equal "#%1:10(-10 -8 -6 -4 -2 0 2 4 6 8)"
      (call-with-output-string (cute display (ra-map! ra11 - ra12 ra13) <>)))
    (test-end (string-append "3 args - " name))))
 `((,(cut ra-map*! ra-slice-for-each-1  <...>) "fe1")
   (,(cut ra-map*! ra-slice-for-each-2  <...>) "fe2")
   (,(cut ra-map*! ra-slice-for-each-3  <...>) "fe3")
   (,(cut ra-map*! ra-slice-for-each-4  <...>) "fe4")
   (,ra-map! "ra-map!")))

; test with enough args to hit the rest list version.
(for-each
 (match-lambda
   ((ra-map! name)
    (test-begin (string-append "4 args - " name))
    (test-equal "#%1:10(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1)"
      (call-with-output-string (cute display (ra-map! ra11 - ra12 ra13 ra12) <>)))
    (test-end (string-append "4 args - " name))))
 `((,(cut ra-map*! ra-slice-for-each-1  <...>) "fe1")
   (,(cut ra-map*! ra-slice-for-each-2  <...>) "fe2")
   (,(cut ra-map*! ra-slice-for-each-3  <...>) "fe3")
   (,(cut ra-map*! ra-slice-for-each-4  <...>) "fe4")
   (,ra-map! "ra-map!")))

(test-begin "ra-for-each")
(test-equal "(10 0 10 0)(9 1 9 1)(8 2 8 2)(7 3 7 3)(6 4 6 4)(5 5 5 5)(4 6 4 6)(3 7 3 7)(2 8 2 8)(1 9 1 9)"
  (call-with-output-string (lambda (s) (ra-for-each (lambda x (display x s)) ra13 ra12 ra13 ra12))))
(test-end "ra-for-each")

(test-begin "ra-swap!")
(let* ((a (ra-copy #t (ra-i 3 2)))
       (root-a (ra-root a))
       (ca (ra-copy a))
       (b (list->ra 2 '((a b) (c d) (e f))))
       (root-b (ra-root b))
       (cb (ra-copy b)))
  (ra-swap! a b)
  (test-eq root-a (ra-root a))
  (test-eq root-b (ra-root b))
  (test-assert (ra-equal? ca b) (ra-equal? cb a)))
(test-end "ra-swap!")


; -----------------------
; ra->list
; -----------------------

(test-equal '(1 2 3) (ra->list (string->ra "#%(1 2 3)")))
(test-equal '(1 2 3) (ra->list (string->ra "#%@1(1 2 3)")))
(test-equal '((1 2) (3 4)) (ra->list (string->ra "#%2((1 2) (3 4))")))
(test-equal '((1 2) (3 4)) (ra->list (string->ra "#%2@1@1((1 2) (3 4))")))
(test-equal '(((1 2 3) (3 4 5)) ((4 5 6) (6 7 8))) (ra->list (string->ra "#%3@1@1@-1(((1 2 3) (3 4 5)) ((4 5 6) (6 7 8)))")))
(test-equal 99 (ra->list (string->ra "#%0(99)")))


; -----------------------
; reader
; -----------------------

(for-each
 (lambda (str)
   (test-equal (format #f "#%3~a:2:2:2(((1 2) (3 4)) ((5 6) (7 8)))" str)
     (ra->string (string->ra (format #f "#%3~a(((1 2) (3 4)) ((5 6) (7 8)))" str))))
   (test-equal (format #f "#%3~a:2:2:2(((1 2) (3 4)) ((5 6) (7 8)))" str)
     (ra->string (string->ra (format #f "#%3~a[((1 2) [3 4]) [[5 6] (7 8)]]" str))))
   (test-equal (format #f "#%2~a:2:2((1 2) (3 4))" str)
     (ra->string (string->ra (format #f "#%2~a((1 2) (3 4))" str))))
   (test-equal (format #f "#%1~a:4(1 2 3 4)" str)
     (ra->string (string->ra (format #f "#%1~a(1 2 3 4)" str))))
   (test-equal (format #f "#%1~a:12(1 2 3 4 5 6 7 8 9 10 11 12)" str)
     (ra->string (string->ra (format #f "#%1~a(1 2 3 4 5 6 7 8 9 10 11 12)" str))))
   (test-equal (format #f "#%2~a@1:2@1:2((1 2) (3 4))" str)
     (ra->string (string->ra (format #f "#%2~a@1@1((1 2) (3 4))" str))))
   (test-equal (format #f "#%2~a@1:2@1:2((1 2) (3 4))" str)
     (ra->string (string->ra (format #f "#%2~a@1@1:2((1 2) (3 4))" str))))
   (test-equal (format #f "#%2~a@-1:2@1:2((1 2) (3 4))" str)
     (ra->string (string->ra (format #f "#%2~a@-1@1((1 2) (3 4))" str)))))
 '("s32" ""))

(test-equal "#%0(#(1 2 3))" (ra->string (string->ra "#%0(#(1 2 3))")))
(test-equal "#%1f64:3(1.0 2.0 3.0)" (ra->string (string->ra "#%1f64(1 2 3)")))

(test-equal "#%0(#%1:3(1 2 3))" (ra->string (string->ra "#%0[#%[1 2 3]]")))
(test-equal "#%1f64:3(1.0 2.0 3.0)" (ra->string (string->ra "#%1f64[1 2 3]")))

(test-equal "#%3:0:0:0()" (ra->string (string->ra "#%3()")))
(test-equal "#%3:2:0:0(() ())" (ra->string (string->ra "#%3(() ())")))
(test-equal "#%3:1:1:0((()))" (ra->string (string->ra "#%3((())))")))


; -----------------------
; list->ra, list->typed-ra
; -----------------------

(test-equal "#%0(99)" (ra->string (list->ra 0 99)))
(test-equal "#%1:3(1 2 3)" (ra->string (list->ra 1 '(1 2 3))))
(test-equal "#%0((1 2 3))" (ra->string (list->ra 0 '(1 2 3))))
(test-equal "#%2:2:2((1 2) (3 4))" (ra->string (list->ra 2 '((1 2) (3 4)))))
(test-equal "#%2:2:3((1 2 3) (4 5 6))" (ra->string (list->ra 2 '((1 2 3) (4 5 6)))))
(test-equal "#%2:3:2((1 2) (3 4) (5 6))" (ra->string (list->ra 2 '((1 2) (3 4) (5 6)))))
(test-equal "#%1:2((1 2) (3 4))" (ra->string (list->ra 1 '((1 2) (3 4)))))
(test-equal "#%0(((1 2) (3 4)))" (ra->string (list->ra 0 '((1 2) (3 4)))))

(test-equal "#%0f64(99.0)" (ra->string (list->typed-ra 'f64 0 99)))
(test-equal "#%1f64:3(1.0 2.0 3.0)" (ra->string (list->typed-ra 'f64 1 '(1 2 3))))
(test-equal "#%2f64:2:2((1.0 2.0) (3.0 4.0))" (ra->string (list->typed-ra 'f64 2 '((1 2) (3 4)))))

(test-equal "#%1f64@1:3(1.0 2.0 3.0)" (ra->string (list->typed-ra 'f64 '(1) '(1 2 3))))
(test-equal "#%1f64@1:3(1.0 2.0 3.0)" (ra->string (list->typed-ra 'f64 '((1 3)) '(1 2 3))))
(test-equal "#%1f64@-1:3(1.0 2.0 3.0)" (ra->string (list->ra 'f64 '(-1) '(1 2 3))))
(test-equal "#%1f64@-1:3(1.0 2.0 3.0)" (ra->string (list->ra 'f64 '((-1 1)) '(1 2 3))))


; -----------------------
; ra-index-map!
; -----------------------

(test-equal "#%2:2:3(((0 0) (0 1) (0 2)) ((1 0) (1 1) (1 2)))"
  (ra->string (let ((x (make-ra 0 2 3))) (ra-index-map! x (lambda x x)))))
(test-equal "#%0(99)"
  (ra->string (let ((x (make-ra 0))) (ra-index-map! x (lambda x 99)))))


; -----------------------
; ra-dimensions, ra-shape
; -----------------------

(test-equal '((-1 3) (0 4))
  (ra-shape (make-ra 'foo '(-1 3) 5)))
(test-equal '((-1 3) 5)
  (ra-dimensions (make-ra 'foo '(-1 3) 5)))


; -----------------------
; raw prefix match
; -----------------------

(let ((ra0 (ra-i 2 3))
      (ra1 (ra-i 2)))
  (test-equal
    "#%2d:2:3((0 1 2) (3 4 5))-#%1d:2(0 1)|"
    (call-with-output-string
     (lambda (o) (ra-slice-for-each 0 (lambda (ra0 ra1) (format o "~a-~a|" ra0 ra1)) ra0 ra1))))
  (test-equal
    "#%1d:3(0 1 2)-#%0d(0)|#%1d:3(3 4 5)-#%0d(1)|"
    (call-with-output-string
     (lambda (o) (ra-slice-for-each 1 (lambda (ra0 ra1) (format o "~a-~a|" ra0 ra1)) ra0 ra1))))
  (test-equal
    "#%0d(0)-#%0d(0)|#%0d(1)-#%0d(0)|#%0d(2)-#%0d(0)|#%0d(3)-#%0d(1)|#%0d(4)-#%0d(1)|#%0d(5)-#%0d(1)|"
    (call-with-output-string
     (lambda (o) (ra-slice-for-each 2 (lambda (ra0 ra1) (format o "~a-~a|" ra0 ra1)) ra0 ra1))))
  (test-assert
   (throws-exception? 'unset-len-for-dim
                      (lambda () (ra-slice-for-each 3 (lambda (ra0 ra1) 0) ra0 ra1)))))


; -----------------------
; prefix match on item (not cell) variants
; -----------------------

(test-assert
 (ra-equal?
  (string->ra "#%1:2(-2 -5)")
  (let ((ra0 (ra-copy #t (ra-i 2 3)))
        (ra1 (ra-copy #t (ra-i 2))))
    (ra-map! ra1 - ra0))))

(test-assert
 (ra-equal?
  (string->ra "#%2:2:3((0 0 0) (-1 -1 -1))")
  (let ((ra0 (ra-copy #t (ra-i 2 3)))
        (ra1 (ra-copy #t (ra-i 2))))
    (ra-map! ra0 - ra1))))

(test-assert
 (ra-equal?
  (string->ra "#%3:4:3:2(((0 0) (1 1) (2 2)) ((3 3) (4 4) (5 5)) ((6 6) (7 7) (8 8)) ((9 9) (10 10) (11 11)))")
  (ra-copy! (make-ra 99 4 3 2) (ra-i 4 3))))

(test-assert
 (ra-equal?
  (string->ra "#%2:3:2((77 77) (77 77) (77 77))")
  (ra-copy! (make-ra 99 3 2) (make-ra 77))))


; -----------------------
; ra-fold
; -----------------------

; these are slow :-/
; numpy (1.16.2)  - time(np.sum(np.arange(1e8)))             ~ 0.15 s
; octave (4.4.1)  - a = time(); b=sum(0:999999999); time()-a ~ 3.1 s
; newra           - ,time (ra-fold + 0 (ra-iota #e1e8))      ~ 4.7 s

(test-equal (* #e1e6 (- #e1e6 1) 1/2) (ra-fold + 0 (ra-iota #e1e6)))
(test-equal (* #e1e6 (- #e1e6 1) 1/2) (ra-fold + 0 (ra-i 100 100 100)))


; -----------------------
; ra-any ra-every
; -----------------------

(test-assert (ra-every positive? (list->ra 1 '(1 2 3 4 5))))
(test-equal -3 (ra-any (lambda (x) (and (negative? x) x)) (list->ra 1 '(1 2 -3 4 5))))
(test-equal #(0 2) (ra-any (lambda (a i j) (and (zero? a) (vector i j)))
                           (list->ra 2 '((3 4 0) (1 2 9)))
                           (ra-iota #f)
                           (ra-transpose (ra-iota #f) 1)))


; -----------------------
; inf dims I - (make-dim #f) = (make-dim #f 0 1)
; -----------------------

(test-assert (ra-equal? (ra-i 3) (make-ra-root (make-aseq) (c-dims 3))))

; inf dimension
(test-assert (not (dim-len (make-dim #f))))

; make an array out of inf root
(test-assert (ra-equal? (ra-i 3) (make-ra-root (make-aseq) (c-dims 3))))

; make an inf array
(let ((rainf (make-ra-root (make-aseq) (c-dims #t))))
  (throws-exception? 'unset-len-for-dim (lambda () (ra-for-each pk rainf)))
  (test-equal #(10 8 6) (ra->array (ra-map! (make-ra #f 3) - (ra-iota 3 10 -1) rainf)))
  (test-equal #(-10 -8 -6) (ra->array (ra-map! (make-ra #f 3) - rainf (ra-iota 3 10 -1)))))

(let ((rainf (ra-i #t 3 4)))
  (test-equal #(1204 1205 1206 1207) (ra->array (ra-copy #t (rainf 100 1))))
  (test-equal '(#f 3 4) (ra-dimensions rainf)))

; properties of inf arrays
(test-equal '((#f #f)) (ra-shape (ra-iota)))


; -----------------------
; inf dims II - (make-dim #f 0 0) = dead axis
; -----------------------

(test-equal
  #2((11 12 13) (21 22 23))
  (ra->array
   (ra-map! (make-ra #f 2 3) +
            (make-ra-root #(10 20) (vector (make-dim 2 0 1) (make-dim #f 0 0)))
            (make-ra-root #(1 2 3) (vector (make-dim #f 0 0) (make-dim 3 0 1))))))

; using generalized transpose

(test-equal
  #2((11 12 13) (21 22 23))
  (ra->array
   (ra-map! (make-ra #f 2 3) +
            (ra-iota 2 10 10)
            (ra-transpose (ra-iota 3 1 1) 1))))

; copying arrays with dead axes

(let ((ra (ra-copy #t (ra-transpose (ra-i 3) 1))))
  (test-equal "#%2:d:3((0 1 2))" (ra->string ra))
  (test-equal #(0 1 2) (ra-root ra)))


; -----------------------
; inf dims III - dimensions can be unbounded in both directions. For these the origin is 0.
; -----------------------

(let ((ii (make-ra-root (make-aseq) (vector (make-dim #f #f 1)))))
  (test-equal 99 (ra-ref ii 99))
  (test-equal -99 (ra-ref ii -99)))


; -----------------------
; from
; -----------------------

; all the tests below pass with fromu = fromb set to ra-from.

(define fromu (@ (newra from) fromu))
(define fromb (@ (newra from) fromb))

(define A (ra-map! (make-ra 0 10 10) + (ra-transpose (ra-iota 10) 1) (ra-iota 10 0 10)))
(define b (ra-i 2))
(define c (make-ra-root (make-aseq 10 2) (vector (make-dim 3 1))))
(define d (ra-i 2 2))

(define iz (ra-i 3))
(ra-from A iz)
(ra-from A 0)
(test-equal "#%2:3:10((0 1 2 3 4 5 6 7 8 9) (10 11 12 13 14 15 16 17 18 19) (20 21 22 23 24 25 26 27 28 29))"
            (ra->string (ra-from A (ra-copy #t (ra-iota 3)))))
(test-equal "#%2:3:2((0 1) (10 11) (20 21))" (ra->string (ra-from A (ra-iota 3) (ra-iota 2))))

; manual examples
(let* ((a (list->ra 2 '((1 2 3) (a b c))))
       (b (ra-from a #t (ra-iota 3 2 -1)))
       (bc (ra-from-copy a #t (ra-iota 3 2 -1)))
       (c (ra-from a #t (list->ra 1 '(2 1 0)))))
  (test-equal "#%2:2:3((3 2 1) (c b a))" (ra->string b) (ra->string c))
  (test-assert (eq? (ra-root a) (ra-root b)))
  (test-assert (not (eq? (ra-root a) (ra-root bc))))
  (test-assert (not (eq? (ra-root a) (ra-root c)))))


; ------------------------
; tests
; ------------------------

(define (test-from id A . i)
; to be able to test integers on fromu
  (let ((ii (map (match-lambda ((? ra? x) x) ((? integer? x) (make-ra x))) i)))
    (test-begin id)
; beatable vs unbeatable
    (let ((X (apply fromb A i))
          (Y (apply fromu A ii)))
      (test-assert (ra-equal? X Y)))
; general as unbeatable (except integers), vs unbeatable
    (let ((X (apply ra-from A (map (lambda (x) (if (ra? x) (ra-copy #t x) x)) i)))
          (Y (apply fromu A ii))
          (Z (apply A i)))
      (test-assert (ra-equal? Y X))
      (test-assert (ra-equal? Y (if (zero? (ra-rank Y)) (make-ra Z) Z))))
; general as beatable, vs beatable
    (let ((X (apply ra-from A i))
          (Y (apply fromb A i))
          (Z (apply A i)))
      (test-assert (ra-equal? Y X))
      (test-assert (ra-equal? Y (if (zero? (ra-rank Y)) (make-ra Z) Z)))
; purely beatable indices preserve the root.
      (test-eq (ra-root A) (ra-root Y))
      (test-eq (ra-root A) (ra-root X))
      (unless (zero? (ra-rank Y))
        (test-assert (ra-root A) (ra-root Z))))
; general as beatable, vs unbeatable
    (let ((X (apply ra-from A i))
          (Y (apply fromu A ii))
          (Z (apply A i)))
      (test-assert (ra-equal? Y X))
      (test-assert (ra-equal? Y (if (zero? (ra-rank Y)) (make-ra Z) Z))))
; general as unbeatable/beatable (2 args), vs unbeatable
    (match i
      ((i j)
       (match ii
         ((ii jj)
          (let ((X1 (ra-from A (if (ra? i) (ra-copy #t i) i) j))
                (X2 (ra-from A i (if (ra? j) (ra-copy #t j) j)))
                (Y (fromu A ii jj))
                (Z1 (A (if (ra? i) (ra-copy #t i) i) j))
                (Z2 (A i (if (ra? j) (ra-copy #t j) j))))
          (test-assert (ra-equal? Y X1))
          (test-assert (ra-equal? Y X2))
          (test-assert (ra-equal? Y (if (zero? (ra-rank Y)) (make-ra Z1) Z1)))
          (test-assert (ra-equal? Y (if (zero? (ra-rank Y)) (make-ra Z2) Z2)))))))
      (else #f))
    (test-end id)))

; ------------------------
; one arg

; rank 0
(test-from "from00" A (make-ra-root (make-aseq 3) (vector)))
(test-from "from01" A 2)
(test-from "from02" A (make-ra 2))

; rank 1
(test-from "from03" A (ra-iota 3))
(test-from "from04" A (make-ra-root (make-aseq) (vector (make-dim 3 1 1))))
(test-from "from05" A (make-ra-root (make-aseq 1 1) (vector (make-dim 3 1 1))))
(test-from "from06" A (make-ra-root (make-aseq 1 2) (vector (make-dim 3 1 1))))
(test-from "from07" A (make-ra-root (make-aseq 1 1) (vector (make-dim 3 1 2))))
(test-from "from08" A (make-ra-root (make-aseq 3 2) (vector (make-dim 2 1 3))))

; rank 2
(test-from "from09" A (ra-i 2 2))
(test-from "from10" A (make-ra-root (make-aseq 3) (vector (make-dim 2 1 2) (make-dim 2 1 3))))


; ------------------------
; two args

; rank 0 0
(test-from "from11" A (make-ra-root (make-aseq 3) (vector))
           (make-ra-root (make-aseq 2) (vector)))
(test-from "from12" A 3 (make-ra-root (make-aseq 2) (vector)))
(test-from "from13" A (make-ra-root (make-aseq 3) (vector)) 2)
(test-from "from14" A 3 2)

; rank 1 1
(test-from "from15" A (ra-iota 3) (ra-iota 2 4))
(test-from "from16" A (make-ra-root (make-aseq) (vector (make-dim 3 1 1)))
           (make-ra-root (make-aseq) (vector (make-dim 3 1 2))))
(test-from "from17" A (make-ra-root (make-aseq 1 1) (vector (make-dim 3 1 1)))
           (make-ra-root (make-aseq 1 2) (vector (make-dim 3 1 1))))
(test-from "from18" A (make-ra-root (make-aseq 1 2) (vector (make-dim 3 1 1)))
           (make-ra-root (make-aseq 1 1) (vector (make-dim 3 1 2))))
(test-from "from19" A (make-ra-root (make-aseq 1 1) (vector (make-dim 3 1 2)))
           (make-ra-root (make-aseq 1 1) (vector (make-dim 3 1 2))))
(test-from "from20" A (make-ra-root (make-aseq 3 2) (vector (make-dim 2 1 3)))
           (make-ra-root (make-aseq 1 1) (vector (make-dim 3 1 2))))

; rank 2 2
(test-from "from21" A (ra-i 3 3) (ra-i 2 2))
(test-from "from22" A (make-ra-root (make-aseq 3) (vector (make-dim 2 1 2) (make-dim 2 1 3)))
           (make-ra-root (make-aseq 3) (vector (make-dim 2 1 2) (make-dim 2 1 3))))

; FIXME if ra-from is used in ra application, then these must give the same result.
(ra-from (ra-i 2 3) (make-ra 0) (make-ra 1))
(ra-cell (ra-i 2 3) 0 1)

; placeholders
(test-begin "from, placeholders")
(let* ((a (ra-i 2 3 4 5 6))
       (ref0 (ra-from a 0 (ra-iota 3)))
       (ref1 (ra-from a 0 (ra-iota 3) 1))
       (ref2 (ra-from a 0 (ra-iota 3) (ra-iota 4) (ra-iota 5) 2)))
  (test-assert (ra-equal? a (ra-from a #t)))
  (test-assert (ra-equal? a (ra-from a (dots 1))))
  (test-assert (ra-equal? a (ra-from a #t #t #t #t #t)))
  (test-assert (ra-equal? a (ra-from a (dots))))
  (test-assert (ra-equal? ref0 (ra-from a 0 #t)))
  (test-assert (ra-equal? ref0 (ra-from a 0 (dots 1))))
  (test-assert (ra-equal? ref0 (ra-from a 0 (dots 0))))
  (test-assert (ra-equal? ref1 (ra-from a 0 #t 1)))
  (test-assert (ra-equal? ref1 (ra-from a 0 (dots 1) 1)))
  (test-assert (ra-equal? ref2 (ra-from a 0 #t #t #t 2)))
  (test-assert (ra-equal? ref2 (ra-from a 0 (dots 3) 2)))
  (test-assert (ra-equal? ref2 (ra-from a 0 (dots) 2))))
(test-end "from, placeholders")


; ------------------------
; other A

(test-begin "from, other cases")

(throws-exception? 'dim-check-out-of-range (lambda () (ra-from (ra-i 4) (ra-iota 6))))
(throws-exception? 'dim-check-out-of-range (lambda () (ra-from (ra-i 4) 4)))

(let ((A (make-ra-root (make-aseq) (vector (make-dim 4 -3 1)))))
  (test-equal 0 (ra-ref (fromu A (make-ra -3))))
  (test-equal 0 (ra-ref (fromb A -3)))
  (test-equal 0 (ra-ref (ra-from A -3)))
  (test-equal 0 (ra-ref (fromu A (make-ra -3))))
  (test-equal 0 (ra-ref (fromb A (make-ra -3))))
  (test-equal 0 (ra-ref (ra-from A (make-ra -3)))))

(let ((A (make-ra-root #(0 1 2 3 4 5 6 7) (vector (make-dim 4 -3 1)))))
  (test-equal 0 (ra-ref (fromu A (make-ra -3))))
  (test-equal 0 (ra-ref (fromb A -3)))
  (test-equal 0 (ra-ref (ra-from A -3)))
  (test-equal 0 (ra-ref (fromu A (make-ra -3))))
  (test-equal 0 (ra-ref (fromb A (make-ra -3))))
  (test-equal 0 (ra-ref (ra-from A (make-ra -3)))))

(test-equal "#%1d:2(0 1)" (ra->string (ra-from (ra-i 4) (ra-iota 2))))
(test-equal "#%1d:4(0 1 2 3)" (ra->string (ra-from (ra-i #t) (ra-iota 4))))
(test-equal "#%1d:4(0 -1 -2 -3)" (ra->string (ra-from (make-ra-root (make-aseq) (vector (make-dim 4 -3 -1))) (ra-iota 4 -3))))

; note that arrays are always indexed upwards, so you cannot have (lo hi) be (#f number).
(throws-exception? 'dim-check-out-of-range (lambda () (ra-from (ra-i #f) (ra-iota 4 -3)))) ; error: -3 < 0
(throws-exception? 'dim-check-out-of-range
                   (lambda () (ra-from (make-ra-root (make-aseq) (vector (make-dim 4 -3 -1)))
                                       (ra-iota 5 -3)))) ; -3+5 > -3+4
(throws-exception? 'dim-check-out-of-range (lambda () (ra-from (ra-i 4) (ra-iota #f))))
(throws-exception? 'dim-check-out-of-range (lambda () (ra-from (ra-copy #t (ra-i 4)) (ra-iota #f))))

(let ((A (make-ra-root (make-aseq) (vector (make-dim 4 1 3) (make-dim 3 1)))))
  (test-equal "#%1d@1:4(1 4 7 10)" (ra->string (ra-from A #t 2)))
  (test-equal "#%1d@1:3(3 4 5)" (ra->string (ra-from A 2 #t))))

(test-end "from, other cases")


; -----------------------
; ra-amend! FIXME needs more tests
; -----------------------

(test-begin "amend!")

(define (ra-I . i) (ra-copy #t (apply ra-i i)))
(define (amend-case A . i) (ra->string (apply ra-amend! A i)))

(test-equal "#%2:2:3((a b c) (3 4 5))" (amend-case (ra-I 2 3) (array->ra #(a b c)) 0))
(test-equal "#%2:2:3((0 1 2) (x y z))" (amend-case (ra-I 2 3) (array->ra #(x y z)) 1))
(test-equal "#%2:2:3((a 1 2) (b 4 5))" (amend-case (ra-I 2 3) (array->ra #(a b)) #t 0))
(test-equal "#%2:2:3((0 x 2) (3 y 5))" (amend-case (ra-I 2 3) (array->ra #(x y)) #t 1))
(test-equal "#%2:2:3((x x x) (x x x))" (amend-case (ra-I 2 3) 'x))
(test-equal "#%2:2:3((x x x) (x x x))" (amend-case (ra-I 2 3) (make-ra 'x)))
(test-equal "#%2:2:3((a a a) (b b b))" (amend-case (ra-I 2 3) (array->ra #(a b))))
(test-equal "#%2:2:3((x y z) (x y z))" (amend-case (ra-I 2 3) (ra-transpose (array->ra #(x y z)) 1)))
(test-equal "#%2:2:3((0 x 2) (3 x 5))" (amend-case (ra-I 2 3) 'x (array->ra #(0 1)) 1))
(test-equal "#%2:2:3((0 x 2) (3 x 5))" (amend-case (ra-I 2 3) 'x (ra-i 2) 1))
(test-equal "#%2:2:3((a b c) (3 4 5))" (amend-case (ra-I 2 3) (array->ra #2((a b c))) (array->ra #(0))))
(test-equal "#%2:2:3((0 1 2) (x y z))" (amend-case (ra-I 2 3) (array->ra #2((x y z))) (array->ra #(1))))
(test-equal "#%2:2:3((a 1 2) (b 4 5))" (amend-case (ra-I 2 3) (array->ra #(a b)) #t 0))
(test-equal "#%2:2:3((0 x 2) (3 y 5))" (amend-case (ra-I 2 3) (array->ra #(x y)) #t 1))
(test-equal "#%2:2:3((a 1 2) (b 4 5))" (amend-case (ra-I 2 3) (array->ra #2((a) (b))) #t (array->ra #(0))))
(test-equal "#%2:2:3((0 x 2) (3 y 5))" (amend-case (ra-I 2 3) (array->ra #2((x) (y))) #t (array->ra #(1))))
(test-equal "#%2:2:3((x y 2) (j k 5))" (amend-case (ra-I 2 3) (array->ra #2((x y) (j k))) #t (array->ra #(0 1))))
(test-equal "#%2:2:3((a 1 b) (3 4 5))" (amend-case (ra-I 2 3) (array->ra #(a b)) 0 (array->ra #(0 2))))
(test-equal "#%2:2:3((0 x 2) (3 y 5))" (amend-case (ra-I 2 3) (array->ra #(x y)) #t 1))
(test-equal "#%1:3(b 2 a)" (amend-case (list->ra 1 '(1 2 3)) (array->ra #(a b)) (array->ra #(2 0))))
(test-equal "#%2:2:3((k 1 j) (y 4 x))" (amend-case (ra-I 2 3) (array->ra #2((x y) (j k))) (array->ra #(1 0)) (array->ra #(2 0))))
(test-equal "#%2:2:3((0 x 2) (3 4 5))" (amend-case (ra-I 2 3) 'x 0 1))

; adapted from guile-ploy
(test-equal "#%1:20(#f a a b b #f c c #f d #f d #f #f #f #f #f #f #f #f)"
            (amend-case (make-ra #f 20) (array->ra #(a b c d)) (array->ra #2((1 2) (3 4) (6 7) (9 11)))))

; with placeholders
(test-equal "#%3:4:3:2(((0 x) (2 x) (4 x)) ((6 x) (8 x) (10 x)) ((12 x) (14 x) (16 x)) ((18 x) (20 x) (22 x)))"
            (amend-case (ra-I 4 3 2) 'x (dots) 1))

(test-end "amend!")


; -----------------------
; ra-ravel
; -----------------------

(test-begin "ravel")

(test-assert (ra-order-c? (ra-i)))
(test-assert (ra-order-c? (ra-i 2)))
(test-assert (ra-order-c? (ra-i 2 3)))
(test-assert (ra-order-c? (ra-i 2 3 4)))
(test-assert (not (ra-order-c? (ra-transpose (ra-i 2 3 4 5) 0 1 3 2))))
(test-assert (not (ra-order-c? (ra-transpose (ra-i 2 3 4 5) 0 1 3 2) 3)))
(test-assert (ra-order-c? (ra-transpose (ra-i 1 2 3 4) 0 1 3 2) 2))
(test-assert (ra-order-c? (ra-transpose (ra-i 1 2 3 4) 0 1 3 2) 1))
(test-assert (not (ra-order-c? (ra-from (ra-i 1 2 3 4) #t #t #t (ra-iota 2 0 2))))) ; last step 1, packed
(test-assert (ra-order-c? (ra-from (ra-i 1 2 3 4) #t #t #t (ra-iota 2 0 2)) 4))
(test-assert (ra-order-c? (ra-tile (ra-i 1 2 3 4) 0 1)))
(test-assert (ra-order-c? (ra-transpose (ra-i 5 1 3 1 4) 0 3 2 1)))
(test-assert (ra-order-c? (ra-transpose (ra-i 2 3 4 5) 1 0 2 3) 2 2))
(test-assert (not (ra-order-c? (ra-transpose (ra-i 2 3 4 5) 1 0 2 3) 2 1)))
(test-assert (ra-order-c? (ra-transpose (ra-i 2 3 4 5) 3 1 2 0) 2 1))
(test-assert (not (ra-order-c? (ra-transpose (ra-i 2 3 4 5) 3 1 2 0) 2 2)))

(test-equal "#%1:18(1 7 13 2 8 14 3 9 15 4 10 16 5 11 17 6 12 18)"
            (ra->string
             (ra-ravel
              (ra-transpose
               (make-ra-root (list->vector (iota 18 1)) (c-dims '(1 3) '(4 9)))
               1 0))))
(let ((ra (make-ra-root (list->vector (iota 18 1)) (c-dims '(1 3) '(4 9)))))
  (test-eq (ra-root ra) (ra-root (ra-ravel ra)))
  (test-equal "#%1:18(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)"
              (ra->string (ra-ravel ra))))
(test-equal "#%1:1(3)" (ra->string (ra-ravel (make-ra 3))))

; partial ravel

(define (test-ravel-n ra keep-root?)
  (let loop ((i (ra-rank ra)))
    (when (>= i 0)
      (let ((ri (ra-ravel ra i)))
        (when keep-root?
          (test-assert (ra-equal? (ra-ravel ri) (ra-ravel ra)))
          (test-eq (ra-root ra) (ra-root ri)))
        (test-equal (ra->list (ra-ravel ri)) (ra->list (ra-ravel ra)))
        (test-equal (+ (ra-rank ra) 1 (- i)) (ra-rank ri)))
      (loop (- i 1)))))

(let* ((ra (ra-i 2 3 4 5 6))
       (rb (ra-from (ra-i 2 3 4 5 6) #t #t #t #t (ra-iota 3 0 2))))
  (test-ravel-n ra #t)
  (test-ravel-n rb #t)
  (test-ravel-n (ra-transpose ra 1 0) #f)
  (test-ravel-n (ra-transpose rb 1 0) #f))

; not prefix, free ravel

(test-equal "#%2d:2:12((0 1 2 3 4 5 6 7 8 9 10 11) (12 13 14 15 16 17 18 19 20 21 22 23))"
            (ra->string (ra-ravel (ra-i 2 3 4) 2 1)))

; not prefix, copy required

(test-assert "#%2:2:12((0 4 8 1 5 9 2 6 10 3 7 11) (12 16 20 13 17 21 14 18 22 15 19 23))"
             (ra->string (ra-ravel (ra-transpose (ra-i 2 3 4) 0 2 1) 2 1)))

(test-end "ravel")


; -----------------------
; ra-reshape
; -----------------------

(test-begin "reshape")

(define (test-reshape k a shape check-shape)
  (let* ((b (apply ra-reshape a k shape)))
    (test-assert (ra-equal? (ra-ravel a) (ra-ravel b)))
    (test-eq (ra-root a) (ra-root b))
    (test-equal check-shape (ra-shape b))))

(test-reshape 0 (ra-i 4 4)
              '(2 2) '((0 1) (0 1) (0 3)))
(test-reshape 1 (ra-i 4 4)
              '(2 2) '((0 3) (0 1) (0 1)))
(test-reshape 0 (ra-i 6)
              '(2 3) '((0 1) (0 2)))
(test-reshape 0 (ra-i 6)
              '((2 4) (3 4)) '((2 4) (3 4)))
(test-reshape 0 (ra-transpose (ra-i 3 2 4) 1 2 0)
              '(2 2) '((0 1) (0 1) (0 2) (0 1)))
(test-reshape 0 (ra-from (make-ra-root (make-aseq) (c-dims '(4 9) '(1 4))) (ra-iota 4 5) (ra-iota 2 1))
              '((1 2) 2) '((1 2) (0 1) (0 1)))
(test-reshape 0 (ra-transpose (make-ra-root (list->vector (iota 18 1)) (c-dims '(1 3) '(4 9))) 1 0)
              '(2 3) '((0 1) (0 2) (1 3)))
(test-reshape 0 (ra-transpose (make-ra-root (make-aseq) (c-dims '(4 9) '(1 4))) 1 0)
              '((1 2) 2) '((1 2) (0 1) (4 9)))

; reshape of inf length - but lo must be set

(let* ((a (ra-iota #f 9))
       (b (ra-reshape a 0 '(2 3))))
  (test-equal '((2 3)) (ra-shape b))
  (test-eq (ra-root a) (ra-root b))
  (test-equal 9 (ra-ref b 2))
  (test-equal 10 (ra-ref b 3)))

; reshape with placeholders

(test-reshape 0 (ra-iota 8) '(#f 2) '((0 3) (0 1)))
(test-reshape 0 (ra-iota 8) '(2 #f) '((0 1) (0 3)))
(test-reshape 0 (ra-iota 8) '((1 4) #f) '((1 4) (0 1)))
(test-reshape 1 (ra-i 5 12) '(2 #f 2) '((0 4) (0 1) (0 2) (0 1)))
(test-reshape 0 (ra-i 12 5) '(2 #f 2) '((0 1) (0 2) (0 1) (0 4)))

(test-end "reshape")


; -----------------------
; ra-tile
; -----------------------

(test-begin "tile")

(define (test-tile a shape check-shape)
  (let* ((b (apply ra-tile a 0 shape)))
    (ra-slice-for-each 2 (lambda (b) (test-assert (ra-equal? a b)))
; enable loop in the dead axis case since there's only that argument.
                       (ra-singletonize b))
    (test-assert (eq? (ra-root a) (ra-root b)))
    (test-equal check-shape (ra-shape b))))

(test-tile (ra-i 5)
           '(2 3) '((0 1) (0 2) (0 4)))
(test-tile (ra-transpose (make-ra-root (make-aseq) (c-dims '(4 9) '(1 4))) 1 0)
           '(2 3) '((0 1) (0 2) (1 4) (4 9)))
(test-tile (ra-transpose (make-ra-root (make-aseq) (c-dims '(4 9) '(1 4))) 1 0)
           '((2 4) (-1 3)) '((2 4) (-1 3) (1 4) (4 9)))
(test-tile (ra-i 5)
           '(2 #f) '((0 1) (0 #f) (0 4)))

(test-end "tile")


; -----------------------
; ra-rotate!
; -----------------------

(test-begin "rotate")

(let ((a (ra-copy #t (ra-i 4 3))))
  (ra-rotate! 0 a)
  (test-assert (ra-equal? (string->ra "#%2:4:3((0 1 2) (3 4 5) (6 7 8) (9 10 11))") a))
  (ra-rotate! 1 a)
  (test-assert (ra-equal? (string->ra "#%2:4:3((3 4 5) (6 7 8) (9 10 11) (0 1 2))") a))
  (ra-rotate! 2 a)
  (test-assert (ra-equal? (string->ra "#%2:4:3((9 10 11) (0 1 2) (3 4 5) (6 7 8))") a))
  (ra-rotate! 3 a)
  (test-assert (ra-equal? (string->ra "#%2:4:3((6 7 8) (9 10 11) (0 1 2) (3 4 5))") a))
  (ra-rotate! -1 a)
  (test-assert (ra-equal? (string->ra "#%2:4:3((3 4 5) (6 7 8) (9 10 11) (0 1 2))") a)))

(let ((a (ra-copy #t (ra-i 4 3))))
  (test-assert (ra-equal? (string->ra "#%2:4:3((0 1 2) (3 4 5) (6 7 8) (9 10 11))") (ra-rotate! 0 a)))
  (test-assert (ra-equal? (string->ra "#%2:4:3((3 4 5) (6 7 8) (9 10 11) (0 1 2))") (ra-rotate! 1 a)))
  (test-assert (ra-equal? (string->ra "#%2:4:3((9 10 11) (0 1 2) (3 4 5) (6 7 8))") (ra-rotate! 2 a)))
  (test-assert (ra-equal? (string->ra "#%2:4:3((6 7 8) (9 10 11) (0 1 2) (3 4 5))") (ra-rotate! 3 a)))
  (test-assert (ra-equal? (string->ra "#%2:4:3((3 4 5) (6 7 8) (9 10 11) (0 1 2))") (ra-rotate! -1 a))))

(let ((a (ra-i 4 3)))
  (test-assert (ra-equal? (string->ra "#%2:4:3((0 1 2) (3 4 5) (6 7 8) (9 10 11))") (ra-rotate 0 a)))
  (test-assert (ra-equal? (string->ra "#%2:4:3((3 4 5) (6 7 8) (9 10 11) (0 1 2))") (ra-rotate 1 a)))
  (test-assert (ra-equal? (string->ra "#%2:4:3((6 7 8) (9 10 11) (0 1 2) (3 4 5))") (ra-rotate 2 a)))
  (test-assert (ra-equal? (string->ra "#%2:4:3((9 10 11) (0 1 2) (3 4 5) (6 7 8))") (ra-rotate 3 a)))
  (test-assert (ra-equal? (string->ra "#%2:4:3((9 10 11) (0 1 2) (3 4 5) (6 7 8))") (ra-rotate -1 a))))

(test-end "rotate")


; -----------------------
; ra-singletonize
; -----------------------

(test-equal '(1 2 1 3) (ra-dimensions (ra-singletonize (ra-transpose (ra-i 2 3) 1 3))))


; -----------------------
; ra-clip
; -----------------------

(test-begin "clip")

(let* ((i0 (ra-transpose (ra-iota) 0))
       (i1 (ra-transpose (ra-iota) 1))
       (i2 (ra-transpose (ra-iota) 2))
       (f (lambda a (format #f "~{~a~}" a)))
       (b (ra-clip (ra-map! (make-ra-new #t 'a (c-dims '(4 6) '(0 2) '(0 4))) f i0 i1 i2)
                   (ra-map! (make-ra-new #t 'b (c-dims '(2 9) '(1 4))) f i0 i1))))
  (test-equal '((4 6) (1 2) (0 4)) (ra-shape b))
  (test-equal
    "#%3@4:3@1:2:5(((\"410\" \"411\" \"412\" \"413\" \"414\") (\"420\" \"421\" \"422\" \"423\" \"424\")) ((\"510\" \"511\" \"512\" \"513\" \"514\") (\"520\" \"521\" \"522\" \"523\" \"524\")) ((\"610\" \"611\" \"612\" \"613\" \"614\") (\"620\" \"621\" \"622\" \"623\" \"624\")))"
    (ra->string b)))

(test-end "clip")


; -----------------------
; a bug in c0f60b1b76118a601715d331c265d1126b0d1e7c & before
; -----------------------

(test-begin "bug in c0f60b1b76118a601715d331c265d1126b0d1e7c")

(let ()
  (define ra0 (make-ra-root #f64(0 1 2 3 4)))
  (define ra1 (ra-copy! (make-typed-ra 'f64 0 5) ra0))
  (test-assert (ra-equal? ra0 ra1))
  (test-equal (ra-ref ra0 4) (ra-ref ra1 4)))

(let ()
  (define ra0 (make-ra-root #u8(0 1 2 3 4)))
  (define ra1 (ra-copy! (make-typed-ra 'u8 0 5) ra0))
  (test-assert (ra-equal? ra0 ra1))
  (test-equal (ra-ref ra0 4) (ra-ref ra1 4)))

(test-end "bug in c0f60b1b76118a601715d331c265d1126b0d1e7c")


; -----------------------
; ra-slice creates a new descriptor with no shared dim vector
; -----------------------

(let ()
  (define ra0 (ra-i 2 3 4))
  (define v0 (vector-copy (ra-dims ra0)))
  (define ra1 (ra-slice ra0))
  (test-assert (not (eq? (ra-dims ra0) (ra-dims ra1))))
  (vector-for-each (lambda (a b) (test-equal a b))
                   v0 (ra-dims ra1))
  (vector-fill! (ra-dims ra1) #f)
  (vector-for-each (lambda (a b) (test-equal a b))
                   v0 (ra-dims ra0)))


; -----------------------
; concatenation
; -----------------------

(test-begin "cat")

(test-assert (ra-equal? (ra-cat #t 0 (ra-i 1 2) (ra-i 2 2))         (array->ra #2((0 1) (0 1) (2 3)))))
(test-assert (ra-equal? (ra-cat #t 0 (ra-iota 2 1) (ra-iota 3 3))   (array->ra #1(1 2 3 4 5))))
(test-assert (ra-equal? (ra-cat #t -1 (ra-iota 2 1) (ra-iota 2 4))  (array->ra #2((1 2) (4 5)))))
(test-assert (ra-equal? (ra-cat #t 1 (ra-iota 2 1) (ra-iota 2 4))   (array->ra #2((1 4) (2 5)))))
(test-assert (ra-equal? (ra-cat #t 0 (make-ra 'a) (ra-iota 2))      (array->ra #1(a 0 1))))
(test-assert (ra-equal? (ra-cat #t 1 (make-ra 'a) (ra-iota 2))      (array->ra #2((a 0) (a 1)))))
(test-assert (ra-equal? (ra-cat #t -1 (make-ra 'a) (ra-iota 2))     (array->ra #2((a a) (0 1)))))
(test-assert (ra-equal? (ra-cat #t 1 (array->ra #(a b)) (ra-i 2 2)) (array->ra #2((a 0 1) (b 2 3)))))
(test-assert (ra-equal? (ra-cat #t 0 (array->ra #(a b)) (ra-i 2 2)) (array->ra #2((a a) (b b) (0 1) (2 3)))))

(test-assert (ra-equal? (ra-cats #t 0 (make-ra 'a) (make-ra 'b) (make-ra 'c))              (array->ra #1(a b c))))
(test-assert (ra-equal? (ra-cats #t 1 (make-ra 'a) (make-ra 'b) (make-ra 'c))              (array->ra #2((a) (b) (c)))))
(test-assert (ra-equal? (ra-cats #t 0 (array->ra #(1 2 3)) (make-ra 4) (array->ra #(5 6))) (array->ra #1(1 2 3 4 5 6))))
(test-assert (ra-equal? (ra-cats #t 0 (array->ra #2((0 1) (2 3))) (array->ra #(a b)))      (array->ra #2((0 1 a b) (2 3 a b)))))
(test-assert (ra-equal? (ra-cats #t 1 (array->ra #2((0 1) (2 3))) (array->ra #(a b)))      (array->ra #2((0 1) (2 3) (a b)))))
(test-assert (ra-equal? (ra-cats #t 0 (array->ra #2((0 1))) (array->ra #(a)))              (array->ra #2((0 1 a)))))
(test-assert (ra-equal? (ra-cats #t -1 (array->ra #(1 2 3)) (array->ra #(a b c)))          (array->ra #2((1 a) (2 b) (3 c)))))
(test-assert (ra-equal? (ra-cats #t -1 (make-ra 'a) (array->ra #(x y z)))                  (array->ra #2((a x) (a y) (a z)))))

; non-zero base indices on the concat axis

(let* ((a0 (ra-i 2 4))
       (a1 (ra-reshape a0 0 '(1 2)))
       (b (ra-i 1 4)))
  (test-assert (ra-equal? (ra-cat #t 0 a0 b) (ra-cat #t 0 a1 b))))

(let* ((a0 (ra-i 4 2))
       (a1 (ra-untranspose (ra-reshape (ra-transpose a0 1 0) 0 '(1 2)) 1 0))
       (b (ra-i 4 1)))
  (test-assert (ra-equal? (ra-cats #t 0 a0 b) (ra-cats #t 0 a1 b))))

; non-zero base indices on a non-concat axis

(let ((a (make-ra 'a '(1 2) '(1 4)))
      (b (make-ra 'b '(1 3) '(1 4))))
  (test-equal "#%2:5@1:4((a a a a) (a a a a) (b b b b) (b b b b) (b b b b))"
    (ra->string (ra-cat #t 0 a b)))
  (test-equal "#%2:5@1:4((a a a a) (a a a a) (b b b b) (b b b b) (b b b b))"
    (ra->string (ra-cats #t 1 a b))))

(test-end "cat")


; -----------------------
; ra-map
; -----------------------

; this requires  ra-slice-for-each-check to fix axes where largest rank argument has dead
(test-equal "#%2f64:3:4((1.0 2.0 3.0 4.0) (2.0 4.0 6.0 8.0) (3.0 6.0 9.0 12.0))"
            (ra->string (ra-map 'f64 * (ra-iota 3 1) (ra-transpose (ra-iota 4 1) 1))))
; lower bounds are respected
(test-equal "#%2:2@1:3((9 9 9) (18 18 18))"
            (ra->string (ra-map #t * (ra-iota 2 1) (ra-transpose (make-ra 9 '(1 3)) 1))))
(test-equal "#%2:3:4((1 2 3 4) (6 7 8 9) (11 12 13 14))"
            (ra->string (ra-map #f + (ra-iota 3 1) (ra-i 3 4))))


; -----------------------
; ra-format
; -----------------------

(test-begin "format")

(define port #f) ; no real test, but at least check that they run. Use #t to demo

(let* ((ra (ra-i 1 8 9 3))
       (ra (ra-map! (ra-copy #t ra) sqrt ra)))
  (ra-format ra port #:fmt "~4,2f"))

(let ((ra (ra-set! (ra-set! (ra-copy #t (ra-i 3 4 3))
                            (ra-i 2 3) 1 2 1)
                   (make-ra-root #(hello world of ras) (c-dims 2 2))
                   2 1 2)))
  (ra-format ra port #:fmt "~a"))

(let ((ra (ra-i 2 2 2 2 2 2 2 2)))
  (ra-format ra port #:compact 1))

(let* ((ra (ra-set! (ra-copy #t (ra-i 7)) (make-ra-root #(hello world of ras) (c-dims 2 2)) 3))
       (ra (ra-tile ra (ra-rank ra) 1)))
  (ra-format ra port))

; example from srfi-163.
(let* ((arr
        #2@1:2@1:3((#2((1 2) (3 4)) 9 #2((3 4) (5 6)))
                   (#(42 43) #2((8 7 6)) #2((90 91) (100 101)))))
       (ra (array->ra arr))
       (ra (ra-map! (ra-copy ra) (lambda (x) (if (array? x) (array->ra x) x)) ra)))
  (ra-format ra port #:prefix? #t #:compact 1)
  (ra-format ra port #:prefix? #t #:compact 0))

(let ((ra (array->ra #2@1:2@1:3((1 2 3) (4 5 6)))))
  (ra-format ra port))

(let ((ra (make-ra 'zero)))
  (ra-format ra port))

; shorter than the prefix; was a bug
(let ((ra (make-ra 'x)))
  (ra-format ra port))

; when size is 0, just print the prefix
(ra-format (ra-i 3 0 4) port)

; when size is unbounded, just print the prefix. Not ideal...
(ra-format (ra-i #t 3 4) port)

; when there are dead axes, print the result of ra-singletonize, except for the prefix. Not ideal...
(ra-format (ra-i 3 #f 4) port)

(test-end "format")


; -----------------------
; the end.
; -----------------------

(define error-count (test-runner-fail-count (test-runner-current)))
(test-end "newra")
(exit error-count)
