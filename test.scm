; -*- mode: scheme; coding: utf-8 -*-
; Replacement for Guile C-based array system - Tests

; (c) Daniel Llorens - 2016-2018
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Run with $GUILE -L mod -s test.scm

(import (newra newra) (newra test) (newra tools) (newra read)
        (srfi :64) (srfi :26) (srfi :8) (only (srfi :1) fold iota)
        (ice-9 match) (only (rnrs base) vector-map))

(define (throws-exception? k thunk)
  (catch #t
    (lambda () (thunk) #f)
    (lambda args (if (eq? k (car args)) args #f))))

(define (ra->string ra) (call-with-output-string (cut display ra <>)))
(define (string->ra s) (call-with-input-string s read))

(set! test-log-to-file #t)
(test-begin "newra")


; -----------------------
; auxiliary functions
; -----------------------

(test-equal
  6 ((@@ (newra newra) vector-fold) (lambda (a c) (+ c (car a))) 0 #((1) (2) (3))))

(test-equal
  #(2 3) ((@@ (newra newra) vector-clip) #(1 2 3 4) 1 3))

; loop-fun from (newra test) FIXME may become ra-index-map!

(define ra0 (make-ra-data (make-dim 1) (c-dims)))
(define ra1 (make-ra-data (make-dim (* 2)) (c-dims 2)))
(define ra2 (make-ra-data (make-dim (* 2 3)) (c-dims 2 3)))
(define ra3 (make-ra-data (make-dim (* 2 3 4)) (c-dims 2 3 4)))

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
       (ra4 (array->ra #0(99))))

  (test-equal (ra->string ra0) "#%1:3(1 2 3)")
  (test-equal (ra->string ra1) "#%1@1:3(1 2 3)")
  (test-equal (ra->string ra2) "#%2:2:2((1 2) (3 4))")
  (test-equal (ra->string ra3) "#%2@1:2@1:2((1 2) (3 4))")
  (test-equal (ra->string ra4) "#%0(99)")

  (test-equal  #(1 2 3)            (ra->array ra0))
  (test-equal  #@1(1 2 3)          (ra->array ra1))
  (test-equal  #2((1 2) (3 4))     (ra->array ra2))
  (test-equal  #2@1@1((1 2) (3 4)) (ra->array ra3))
  (test-equal  #0(99)              (ra->array ra4))

; dead axes

  (test-equal "#%2:d:10((0 1 2 3 4 5 6 7 8 9))" (ra->string (ra-transpose (ra-i 10) 1))))


; -----------------------
; ra-iota, ra-i
; -----------------------

(test-equal "#%3:2:3:4(((0 1 2 3) (4 5 6 7) (8 9 10 11)) ((12 13 14 15) (16 17 18 19) (20 21 22 23)))"
  (ra->string (ra-i 2 3 4)))

(test-equal "#%3:2:0:4(() ())"
  (ra->string (ra-i 2 0 4)))

(test-equal "#%0(0)"
  (ra->string (ra-i)))

(test-equal "#%1:4(9 8 7 6)"
  (ra->string (ra-iota 4 9 -1)))

(test-equal "#%1:4(9 10 11 12)"
  (ra->string (ra-iota 4 9)))

(test-equal "#%1:0()"
  (ra->string (ra-iota 0)))

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
  (array-index-map! (ra-data ra5) (lambda i i))
  (test-equal (ra->string ra5) "#%2@1:3@1:2(((0) (1)) ((2) (3)) ((4) (5)))")
  (test-equal 3 (ra-length ra5)))

(let* ((ra5 (make-typed-ra 's64 0 '(1 3) '(1 2))))
  (array-index-map! (ra-data ra5) (lambda i (car i)))
  (test-equal (ra->string ra5) "#%2s64@1:3@1:2((0 1) (2 3) (4 5))")
  (test-equal 3 (ra-length ra5)))


; -----------------------
; make-ra-data
; -----------------------

(define ra6 (make-ra-data #(1 2 3 4 5 6) (c-dims '(1 2) '(1 3))))
(test-equal (ra->string ra6) "#%2@1:2@1:3((1 2 3) (4 5 6))")
(define ra7a (make-ra-data (make-dim 6 1) (c-dims '(1 2) '(1 3))))
(test-equal (ra->string ra7a) "#%2@1:2@1:3((1 2 3) (4 5 6))")
(define ra7b (make-ra-raw #(1 4 2 5 3 6) -3 `#(,(make-dim 2 1 1) ,(make-dim 3 1 2))))
(test-equal (ra->string ra7b) "#%2@1:2@1:3((1 2 3) (4 5 6))")

(test-equal 2 (ra-length ra6))
(test-equal 2 (ra-length ra7a))
(test-equal 2 (ra-length ra7b))


; -----------------------
; ra-slice, ra-ref, ra-cell
; -----------------------

(define ra8 (make-ra-data (make-dim 6 1) (c-dims '(1 2) '(1 3))))
(test-equal 2 (ra-length ra8))

(test-equal (ra->string (ra-cell ra8)) "#%2@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra-cell ra8 1)) "#%1@1:3(1 2 3)")
(test-equal (ra->string (ra-cell ra8 2)) "#%1@1:3(4 5 6)")
(test-equal 5 (ra-cell ra8 2 2))

; applicable!
(test-equal (ra->string (ra8)) "#%2@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra8 1)) "#%1@1:3(1 2 3)")
(test-equal (ra->string (ra8 2)) "#%1@1:3(4 5 6)")
(test-equal 5 (ra8 2 2))

(test-equal (ra->string (ra-slice ra8)) "#%2@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra-slice ra8 1)) "#%1@1:3(1 2 3)")
(test-equal (ra->string (ra-slice ra8 2)) "#%1@1:3(4 5 6)")
(test-equal (ra->string (ra-slice ra8 2 2)) "#%0(5)")
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

(test-equal (ra->string (ra-transpose ra7a 1 0)) "#%2@1:3@1:2((1 4) (2 5) (3 6))")
(test-equal (ra->string (ra-transpose ra7b 1 0)) "#%2@1:3@1:2((1 4) (2 5) (3 6))")
(test-equal (ra->string (ra-transpose ra7a 0 1)) "#%2@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra-transpose ra7b 0 1)) "#%2@1:2@1:3((1 2 3) (4 5 6))")
(test-equal (ra->string (ra-transpose ra7a 0 0)) "#%1@1:2(1 5)")
(test-equal (ra->string (ra-transpose ra7b 0 0)) "#%1@1:2(1 5)")
(test-assert (throws-exception? 'bad-number-of-axes (lambda () (ra-transpose (ra-iota 3) 0 1))))


; -----------------------
; ra-slice-for-each
; -----------------------

(define ra-empty0 (make-ra-data (make-dim 6 1) (c-dims '(1 0) '(2 1))))
(define ra-empty1 (make-ra-data (make-dim 6 1) (c-dims '(1 1) '(2 1))))
(define ra-empty2 (make-ra-data (make-dim 6 1) (c-dims '(1 0) '(2 2))))
(test-equal 0 (ra-length ra-empty0))
(test-equal 1 (ra-length ra-empty1))
(test-equal 0 (ra-length ra-empty2))

(for-each
 (lambda (ra-slice-for-each)
   (test-begin (procedure-name ra-slice-for-each))

   (test-equal "#%2@1:0@2:0()\n"
     (call-with-output-string
      (lambda (s) (ra-slice-for-each 0 (lambda (o) (format s "~a\n" o)) ra-empty0))))
   (test-equal "#%2@1:1@2:0(())\n"
     (call-with-output-string
      (lambda (s) (ra-slice-for-each 0 (lambda (o) (format s "~a\n" o)) ra-empty1))))
   (test-equal "#%2@1:0@2:1()\n"
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

   (test-equal "(#%2@1:2@1:3((1 2 3) (4 5 6)) #%2@1:2@1:3((1 2 3) (4 5 6)) #%2@1:2@1:3((1 2 3) (4 5 6)))\n"
     (call-with-output-string
      (lambda (s) (ra-slice-for-each 0 (lambda x (format s "~a\n" x)) ra6 ra7a ra7b))))
   (test-equal "(#%1@1:3(1 2 3) #%1@1:3(1 2 3) #%1@1:3(1 2 3))\n(#%1@1:3(4 5 6) #%1@1:3(4 5 6) #%1@1:3(4 5 6))\n"
     (call-with-output-string
      (lambda (s) (ra-slice-for-each 1 (lambda x (format s "~a\n" x)) ra6 ra7a ra7b))))
   (test-equal "(#%0(1) #%0(1) #%0(1))\n(#%0(2) #%0(2) #%0(2))\n(#%0(3) #%0(3) #%0(3))\n(#%0(4) #%0(4) #%0(4))\n(#%0(5) #%0(5) #%0(5))\n(#%0(6) #%0(6) #%0(6))\n"
     (call-with-output-string
      (lambda (s) (ra-slice-for-each 2 (lambda x (format s "~a\n" x)) ra6 ra7a ra7b))))

   (test-end (procedure-name ra-slice-for-each)))
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
         (test-end (format #f "~a" name)))))
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

(define ra9 (make-ra-data (make-vector 6) (c-dims '(-1 0) '(1 3))))
(set! (ra9 -1 1) 99)
(set! (ra9 -1 2) 77)
(set! (ra9 -1 3) 88)
(set! (ra9 0 1) 33)
(set! (ra9 0 2) 11)
(set! (ra9 0 3) 22)
(test-equal (ra->string ra9) "#%2@-1:2@1:3((99 77 88) (33 11 22))")
(test-equal 2 (ra-length ra9))
(test-equal #2@-1:2@1:3((99 77 88) (33 44 22)) (ra->array (set! (ra9 0 2) 44)))


; -----------------------
; test through the ra-map! interface, also ra-copy!, ra-fill!, ra-equal?
; -----------------------

(define ra11 (make-ra-new #t 0 (c-dims 10)))
(define ra12 (make-ra-data (make-dim 10) (c-dims 10)))
(define ra13 (make-ra-data (make-dim 10 10 -1) (c-dims 10)))

(test-equal 10 (ra-length ra11))
(test-equal 10 (ra-length ra12))
(test-equal 10 (ra-length ra13))

(test-equal "#%1:10(0 1 2 3 4 5 6 7 8 9)" (ra->string (ra-copy! ra11 ra12)))
(let ((ra5a (make-ra-new #t 0 (c-dims '(1 2) '(1 3))))
      (ra6a (make-ra-data (vector-copy #(1 2 3 4 5 6)) (c-dims '(1 2) '(1 3)))))
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
  (test-assert (ra-equal? (make-ra-data #(99 99 99 99) (c-dims '(1 2) '(1 2)))
                          (ra-fill! (make-ra-data (vector 1 2 3 4) (c-dims '(1 2) '(1 2))) 99))))

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

; test with enough args to hit the arglist version.
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
    "#%2:2:3((0 1 2) (3 4 5))-#%1:2(0 1)|"
    (call-with-output-string
     (lambda (o) (ra-slice-for-each 0 (lambda (ra0 ra1) (format o "~a-~a|" ra0 ra1)) ra0 ra1))))
  (test-equal
    "#%1:3(0 1 2)-#%0(0)|#%1:3(3 4 5)-#%0(1)|"
    (call-with-output-string
     (lambda (o) (ra-slice-for-each 1 (lambda (ra0 ra1) (format o "~a-~a|" ra0 ra1)) ra0 ra1))))
  (test-equal
    "#%0(0)-#%0(0)|#%0(1)-#%0(0)|#%0(2)-#%0(0)|#%0(3)-#%0(1)|#%0(4)-#%0(1)|#%0(5)-#%0(1)|"
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


; -----------------------
; inf dims I - (make-dim #f) = (make-dim #f 0 1)
; -----------------------

(test-assert (ra-equal? (ra-i 3) (make-ra-data (make-dim #f) (c-dims 3))))

; inf index vector
(test-assert (not (dim-len (make-dim #f))))

; make an array out of inf index vector
(test-assert (ra-equal? (ra-i 3) (make-ra-data (make-dim #f) (c-dims 3))))

; make an inf array
(let ((rainf (make-ra-data (make-dim #f) (c-dims #f))))
  (throws-exception? 'unset-len-for-dim (lambda () (ra-for-each pk rainf)))
  (test-equal #(10 8 6) (ra->array (ra-map! (make-ra #f 3) - (ra-iota 3 10 -1) rainf)))
  (test-equal #(-10 -8 -6) (ra->array (ra-map! (make-ra #f 3) - rainf (ra-iota 3 10 -1)))))

(let ((rainf (ra-i #f 3 4)))
  (test-equal #(1204 1205 1206 1207) (ra->array (ra-copy #t (rainf 100 1))))
  (test-equal '(#f 3 4) (ra-dimensions rainf)))


; -----------------------
; inf dims II - (make-dim #f 0 0) = dead axis
; -----------------------

(test-equal
  #2((11 12 13) (21 22 23))
  (ra->array
   (ra-map! (make-ra #f 2 3) +
            (make-ra-data #(10 20) (vector (make-dim 2 0 1) (make-dim #f 0 0)))
            (make-ra-data #(1 2 3) (vector (make-dim #f 0 0) (make-dim 3 0 1))))))

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
  (test-equal #(0 1 2) (ra-data ra)))


; -----------------------
; the end.
; -----------------------

(test-end "newra")
(exit (test-runner-fail-count (test-runner-current)))
