
; Replacement for Guile C-based array system - Benchmarks
; (c) Daniel Llorens - 2016-2017
; Run with $GUILE -L mod -s bench.scm

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(import (newra newra) (newra print) (newra tools) (newra test)
        (only (rnrs base) vector-map)
        (srfi srfi-26) (srfi srfi-8) (only (srfi srfi-1) fold iota)
        (ice-9 match) (ice-9 format))


; -----------------------
; benchmarks
; -----------------------

(define type #t)
(define m #e1e5)
(format #t "\nra-slice-for-each array-slice-for-each ra-map! array-map!\n==========\n")
(for-each
  (lambda (nargs)
    (format #t "\n~a args\n---------\n" nargs)
    (for-each
      (lambda (rank)
        (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
               (nn (make-list rank n))
               (len (fold * 1 nn))
               (scale (* 1e3 (/ m len)))
               (ra20 (apply make-ra-new type *unspecified* nn))
               (ra21 (ra-map*! ra-slice-for-each (apply make-ra-new type 0 nn) (lambda () (random n))))
               (ra22 (ra-map*! ra-slice-for-each (apply make-ra-new type 0 nn) (lambda () (random n))))
               (a20 (ra->array ra20))
               (a21 (ra->array ra21))
               (a22 (ra->array ra22)))
          (format #t "rank ~a (nn ~a)\n" rank nn)
          (case nargs
            ((3)
             ;; (format #t "1~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-1 ra20 - ra21 ra22))))
             (format #t "2~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-2 ra20 - ra21 ra22))))
             (format #t "3~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-3 ra20 - ra21 ra22))))
             (format #t "4~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-4 ra20 - ra21 ra22))))
             (format #t "5~20t~9,4f\n" (* scale (time (array-map*! a20 - a21 a22))))
             (format #t "6~20t~9,4f\n" (* scale (time (ra-map! ra20 - ra21 ra22))))
             (format #t "7~20t~9,4f\n" (* scale (time (array-map! a20 - a21 a22)))))
            ((2)
             ;; (format #t "1~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-1 ra20 - ra21))))
             (format #t "2~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-2 ra20 - ra21))))
             (format #t "3~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-3 ra20 - ra21))))
             (format #t "4~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-4 ra20 - ra21))))
             (format #t "5~20t~9,4f\n" (* scale (time (array-map*! a20 - a21))))
             (format #t "6~20t~9,4f\n" (* scale (time (ra-map! ra20 - ra21))))
             (format #t "7~20t~9,4f\n" (* scale (time (array-map! a20 - a21)))))
            ((1)
             ;; (format #t "1~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-1 ra20 (lambda () (random n))))))
             (format #t "2~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-2 ra20 (lambda () (random n))))))
             (format #t "3~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-3 ra20 (lambda () (random n))))))
             (format #t "4~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-4 ra20 (lambda () (random n))))))
             (format #t "5~20t~9,4f\n" (* scale (time (array-map*! a20 (lambda () (random n))))))
             (format #t "6~20t~9,4f\n" (* scale (time (ra-map! ra20 (lambda () (random n))))))
             (format #t "7~20t~9,4f\n" (* scale (time (array-map! a20 (lambda () (random n))))))))))
      (iota 6 1)))
  (iota 3 1))

(define m #e5e5)
(format #t "\nra-copy! array-copy!\n==========\n")
(for-each
  (lambda (typesrc typedst)
    (format #t "\ntype src ~a -> type dst ~a\n---------\n" typesrc typedst)
    (for-each
      (lambda (rank)
        (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
               (nn (make-list rank n))
               (len (fold * 1 nn))
               (scale (* 1e3 (/ m len)))
               (ra20 (apply make-ra-new typesrc *unspecified* nn))
               (ra21 (ra-map! (apply make-ra-new typedst 0 nn) (lambda () (random n))))
               (a20 (ra->array ra20))
               (a21 (ra->array ra21)))
          (format #t "rank ~a (nn ~a)\n" rank nn)
          (format #t "ra~20t~9,4f\n" (* scale (time (ra-copy! ra21 ra20))))
          (format #t "array~20t~9,4f\n" (* scale (time (array-copy! a21 a20))))))
      (iota 6 1)))
  (list #t 'f64 #t)
  (list #t 'f64 'f64))

(define m #e5e5)
(format #t "\nra-fill! array-fill!\n==========\n")
(for-each
  (lambda (type)
    (format #t "\ntype dst ~a\n----------\n" type)
    (for-each
      (lambda (rank)
        (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
               (nn (make-list rank n))
               (len (fold * 1 nn))
               (scale (* 1e3 (/ m len)))
               (ra20 (apply make-ra-new type *unspecified* nn))
               (a20 (ra->array ra20)))
          (format #t "rank ~a (nn ~a)\n" rank nn)
          (format #t "ra1~20t~9,4f\n" (* scale (time (ra-fill-1! ra20 77))))
          (format #t "ra2~20t~9,4f\n" (* scale (time (ra-fill-2! ra20 77))))
          (format #t "array~20t~9,4f\n" (* scale (time (array-fill! a20 77))))))
      (iota 6 1)))
  (list #t 'f64 'u8))

(define m #e5e5)
(format #t "\nra-equal? array-equal?\n==========\n")
(for-each
  (lambda (type)
    (format #t "\ntype dst ~a\n----------\n" type)
    (for-each
      (lambda (rank)
        (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
               (nn (make-list rank n))
               (len (fold * 1 nn))
               (scale (* 1e3 (/ m len)))
               (ra20 (ra-map! (apply make-ra-new type 0 nn) (lambda () (random n))))
               (ra21 (ra-copy! ra20 (apply make-ra-new type 0 nn)))
               (a20 (ra->array ra20))
               (a21 (ra->array ra21)))
          (format #t "rank ~a (nn ~a)\n" rank nn)
          (format #t "ra~20t~9,4f\n" (* scale (time (ra-equal? ra20 ra21))))
          (format #t "array~20t~9,4f\n" (* scale (time (array-equal? a20 a21))))))
      (iota 6 1)))
  (list #t 'f64))

(define m #e1e4)
(format #t "\nprinting\n==========\n")
(for-each
  (lambda (type)
    (format #t "\ntype dst ~a\n----------\n" type)
    (for-each
      (lambda (rank)
        (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
               (nn (make-list rank n))
               (len (fold * 1 nn))
               (scale (* 1e3 (/ m len)))
               (ra (ra-map! (apply make-ra-new type 0 nn) (lambda () (random n))))
               (a (ra->array ra)))
          (format #t "rank ~a (nn ~a)\n" rank nn)
          (format #t "ra~20t~9,4f\n" (* scale (time (call-with-output-file "/dev/null" (cut display ra <>)))))
          (format #t "array1~20t~9,4f\n" (* scale (time (call-with-output-file "/dev/null" (cut array-print* a <>)))))
          (format #t "array2~20t~9,4f\n" (* scale (time (call-with-output-file "/dev/null" (cut display a <>)))))))
      (iota 6 1)))
  (list #t 'f64))

(define m #e1e4)
(format #t "\nreading\n==========\n")
(for-each
  (lambda (type)
    (format #t "\ntype dst ~a\n----------\n" type)
    (for-each
      (lambda (rank)
        (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
               (nn (make-list rank n))
               (len (fold * 1 nn))
               (scale (* 1e3 (/ m len)))
               (ra (ra-map! (apply make-ra-new type 0 nn) (lambda () (random n))))
               (sra1 (call-with-output-string (cut (@@ (newra print) ra-print) ra <> #:dims? #t)))
               (sra2 (call-with-output-string (cut (@@ (newra print) ra-print) ra <> #:dims? #f)))
               (a (ra->array ra))
               (sa (call-with-output-string (cut display a <>))))
          (format #t "rank ~a (nn ~a)\n" rank nn)
          (let ((rb #f) (b #f))
            (format #t "ra1~20t~9,4f\n" (* scale (time (set! rb (call-with-input-string sra1 read)))))
            (format #t "ra2~20t~9,4f\n" (* scale (time (set! rb (call-with-input-string sra2 read)))))
            (format #t "array~20t~9,4f\n" (* scale (time (set! b (call-with-input-string sa read)))))
            (unless (array-equal? (ra->array rb) b) (throw 'bad-reading-benchmark)))))
      (iota 6 1)))
  (list #t 'f64))

(define m #e1e5)
(format #t "\nlist->ra\n==========\n")
(for-each
  (lambda (type)
    (format #t "\ntype dst ~a\n----------\n" type)
    (for-each
      (lambda (rank)
        (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
               (nn (make-list rank n))
               (len (fold * 1 nn))
               (scale (* 1e3 (/ m len)))
               (ra (ra-map! (apply make-ra-new type 0 nn) (lambda () (random n))))
               (la (ra->list ra))
               (shape (map (lambda (len) (list 0 (- len 1))) nn)))
          (format #t "rank ~a (nn ~a)\n" rank nn)
          (let ((rb #f) (b #f))
            (format #t "ra1~20t~9,4f\n" (* scale (time (set! rb (list->ra rank la)))))
            (format #t "array2~20t~9,4f\n" (* scale (time (set! b (list->array rank la)))))
            (unless (array-equal? (ra->array rb) b) (throw 'bad-ra->list-benchmark))
            (format #t "ra/shape3~20t~9,4f\n" (* scale (time (set! rb (list->typed-ra #t shape la)))))
            (format #t "array/shape4~20t~9,4f\n" (* scale (time (set! b (list->typed-array #t shape la)))))
            (unless (array-equal? (ra->array rb) b) (throw 'bad-ra->list-benchmark)))))
      (iota 6 1)))
  (list #t 'f64))


; -----------------------
; some profiling...
; -----------------------

(import (statprof))

(define m #e5e4)
(define type #t)
(let* ((rank 3)
       (n (inexact->exact (ceiling (expt (* 10 m) (/ rank)))))
       (nn (make-list rank n))
       (ra0 (apply make-ra-new type *unspecified* nn))
       (ra1 (ra-map! (apply make-ra-new type 0 nn) (lambda () (random n))))
       (ra2 (ra-map! (apply make-ra-new type 0 nn) (lambda () (random n))))
       (s (call-with-output-string (cut display ra1 <>)))
       (prof (lambda () (ra-map! ra0 * ra1 ra2)))
       (prof (lambda () (call-with-input-string s read)))
       (prof (lambda () (ra-fill! ra0 99))))
  (statprof prof #:count-calls? #t)
  prof)
