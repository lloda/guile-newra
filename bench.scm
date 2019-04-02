
; (c) Daniel Llorens - 2016-2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Replacement for Guile C-based array system - Benchmarks
; Run with $GUILE -L mod -s bench.scm

(import (newra newra) (newra print) (newra tools) (newra test) (newra read) (newra lib)
        (ice-9 popen) (ice-9 rdelim)
        (only (rnrs base) vector-map)
        (srfi srfi-26) (srfi srfi-8) (only (srfi srfi-1) fold iota)
        (ice-9 match) (ice-9 format))

(define (command-output cmd . args)
  (let* ((p (apply open-pipe* OPEN_READ cmd args))
         (s (read-delimited "" p))
         (ec (status:exit-val (close-pipe p))))
    (values s ec)))

(format #t "Guile ~a\n~!" (version))
(format #t "newra ~a\n~!" (command-output "git" "describe" "--always" "--dirty"))

(define (format-header . x)
  (apply format #t
    (string-append (apply string-append "~30t" (map (const "~15@a") x))
                   "\n")
    x))

(define (format-line . x)
  (apply format #t
    (string-append (apply string-append "~30t" (map (const "~15,3f") x))
                   "\n")
    x))


; -----------------------
; benchmarks
; -----------------------

(define m #e1e5)
(format #t "\nra-cell (applicable function) / array-ref\n==================\n")
(for-each
 (lambda (type)
   (format #t "\ntype ~a\n---------" type)
   (format-header "ra-cell" "ra-ref" "ra-appl" "array")
   (for-each
    (lambda (rank)
      (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
             (nn (make-list rank n))
             (len (fold * 1 nn))
             (scale (* 1e3 (/ m len)))
             (ra (as-ra (make-ra-data (make-dim len) (apply c-dims nn)) #:type type))
             (a (ra->array ra))
             (ras 0)
             (as 0)
; FIXME test ra-ref / ra-cell / (ra ...)
             (raf-cell (case-lambda ((i) (set! ras (+ ras (ra-ref ra i))))
                                    ((i j) (set! ras (+ ras (ra-ref ra i j))))
                                    ((i j k) (set! ras (+ ras (ra-ref ra i j k))))
                                    ((i j k l) (set! ras (+ ras (ra-ref ra i j k l))))
                                    ((i j k l m) (set! ras (+ ras (ra-ref ra i j k l m))))))
             (raf-ref (case-lambda ((i) (set! ras (+ ras (ra-cell ra i))))
                                   ((i j) (set! ras (+ ras (ra-cell ra i j))))
                                   ((i j k) (set! ras (+ ras (ra-cell ra i j k))))
                                   ((i j k l) (set! ras (+ ras (ra-cell ra i j k l))))
                                   ((i j k l m) (set! ras (+ ras (ra-cell ra i j k l m))))))
             (raf-appl (case-lambda ((i) (set! ras (+ ras (ra i))))
                                    ((i j) (set! ras (+ ras (ra i j))))
                                    ((i j k) (set! ras (+ ras (ra i j k))))
                                    ((i j k l) (set! ras (+ ras (ra i j k l))))
                                    ((i j k l m) (set! ras (+ ras (ra i j k l m))))))
             (af (case-lambda ((i) (set! as (+ as (array-ref a i))))
                              ((i j) (set! as (+ as (array-ref a i j))))
                              ((i j k) (set! as (+ ras (array-ref a i j k))))
                              ((i j k l) (set! as (+ ras (array-ref a i j k l))))
                              ((i j k l m) (set! as (+ ras (array-ref a i j k l m)))))))
        (unless (= ras as) (throw 'error-in-ra-cell-array-ref-check))
        (format #t "rank ~a ~a:" rank nn)
        (format-line (* scale (time (ra-loop ra raf-cell)))
                     (* scale (time (ra-loop ra raf-ref)))
                     (* scale (time (ra-loop ra raf-appl)))
                     (* scale (time (array-loop a af))))))
    (iota 5 1)))
 '(#t f64))

(define m #e1e5)
(format #t "\nra-slice-for-each array-slice-for-each ra-map! array-map!\n==================\n")
(for-each
 (lambda (type)
   (for-each
    (lambda (nargs)
      (format #t "\ntype ~a ~a args\n---------" type nargs)
      (format-header "ra*" "array*" "ra" "array")
      (for-each
       (lambda (rank)
         (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
                (nn (make-list rank n))
                (len (fold * 1 nn))
                (scale (* 1e3 (/ m len)))
                (ra20 (make-ra-new type *unspecified* (apply c-dims nn)))
                (ra21 (ra-map*! ra-slice-for-each (make-ra-new type 0 (apply c-dims nn)) (lambda () (random n))))
                (ra22 (ra-map*! ra-slice-for-each (make-ra-new type 0 (apply c-dims nn)) (lambda () (random n))))
                (a20 (ra->array ra20))
                (a21 (ra->array ra21))
                (a22 (ra->array ra22)))
           (format #t "rank ~a ~a:" rank nn)
           (case nargs
             ((3)
              (format-line (* scale (time (ra-map*! ra-slice-for-each-4 ra20 - ra21 ra22)))
                           (* scale (time (array-map*! a20 - a21 a22)))
                           (* scale (time (ra-map! ra20 - ra21 ra22)))
                           (* scale (time (array-map! a20 - a21 a22)))))
             ((2)
              (format-line (* scale (time (ra-map*! ra-slice-for-each-4 ra20 - ra21)))
                           (* scale (time (array-map*! a20 - a21)))
                           (* scale (time (ra-map! ra20 - ra21)))
                           (* scale (time (array-map! a20 - a21)))))
             ((1)
              (format-line (* scale (time (ra-map*! ra-slice-for-each-4 ra20 (lambda () (random n)))))
                           (* scale (time (array-map*! a20 (lambda () (random n)))))
                           (* scale (time (ra-map! ra20 (lambda () (random n)))))
                           (* scale (time (array-map! a20 (lambda () (random n))))))))))
       (iota 6 1)))
    (iota 3 1)))
 '(#t f64))

(define m #e5e5)
(format #t "\nra-copy! array-copy!\n==================\n")
(for-each
    (lambda (typesrc typedst)
      (format #t "\ntype src ~a -> type dst ~a\n---------" typesrc typedst)
      (format-header "ra" "array")
      (for-each
          (lambda (rank)
            (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
                   (nn (make-list rank n))
                   (len (fold * 1 nn))
                   (scale (* 1e3 (/ m len)))
                   (ra20 (make-ra-new typesrc *unspecified* (apply c-dims nn)))
                   (ra21 (ra-map! (make-ra-new typedst 0 (apply c-dims nn)) (lambda () (random n))))
                   (a20 (ra->array ra20))
                   (a21 (ra->array ra21)))
              (format #t "rank ~a ~a:" rank nn)
              (format-line (* scale (time (ra-copy! ra21 ra20)))
                           (* scale (time (array-copy! a21 a20))))))
        (iota 6 1)))
  (list #t 'f64 #t)
  (list #t 'f64 'f64))

(define m #e5e5)
(format #t "\nra-fill! array-fill!\n==================\n")
(for-each
    (lambda (type)
      (format #t "\ntype dst ~a\n----------" type)
      (format-header "ra" "array")
      (for-each
          (lambda (rank)
            (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
                   (nn (make-list rank n))
                   (len (fold * 1 nn))
                   (scale (* 1e3 (/ m len)))
                   (ra20 (make-ra-new type *unspecified* (apply c-dims nn)))
                   (a20 (ra->array ra20)))
              (format #t "rank ~a ~a:" rank nn)
              (format-line (* scale (time (ra-fill! ra20 77)))
                           (* scale (time (array-fill! a20 77))))))
        (iota 6 1)))
  (list #t 'f64 'u8))

(define m #e5e5)
(format #t "\nra-equal? array-equal?\n==================\n")
(for-each
    (lambda (type)
      (format #t "\ntype dst ~a\n----------" type)
      (format-header "ra" "array")
      (for-each
          (lambda (rank)
            (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
                   (nn (make-list rank n))
                   (len (fold * 1 nn))
                   (scale (* 1e3 (/ m len)))
                   (ra20 (ra-map! (make-ra-new type 0 (apply c-dims nn)) (lambda () (random n))))
                   (ra21 (ra-copy! ra20 (make-ra-new type 0 (apply c-dims nn))))
                   (a20 (ra->array ra20))
                   (a21 (ra->array ra21)))
              (format #t "rank ~a ~a:" rank nn)
              (format-line (* scale (time (ra-equal? ra20 ra21)))
                           (* scale (time (array-equal? a20 a21))))))
        (iota 6 1)))
  (list #t 'f64))

(define m #e1e4)
(format #t "\nprinting\n==================\n")
(for-each
    (lambda (type)
      (format #t "\ntype dst ~a\n----------" type)
      (format-header "ra" "array1" "array2")
      (for-each
          (lambda (rank)
            (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
                   (nn (make-list rank n))
                   (len (fold * 1 nn))
                   (scale (* 1e3 (/ m len)))
                   (ra (ra-map! (make-ra-new type 0 (apply c-dims nn)) (lambda () (random n))))
                   (a (ra->array ra)))
              (format #t "rank ~a ~a:" rank nn)
              (format-line (* scale (time (call-with-output-file "/dev/null" (cut display ra <>))))
                           (* scale (time (call-with-output-file "/dev/null" (cut array-print* a <>))))
                           (* scale (time (call-with-output-file "/dev/null" (cut display a <>)))))))
        (iota 6 1)))
  (list #t 'f64))

(define m #e1e4)
(format #t "\nreading\n==================\n")
(for-each
    (lambda (type)
      (format #t "\ntype dst ~a\n----------" type)
      (format-header "ra1" "ra2" "array")
      (for-each
          (lambda (rank)
            (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
                   (nn (make-list rank n))
                   (len (fold * 1 nn))
                   (scale (* 1e3 (/ m len)))
                   (ra (ra-map! (make-ra-new type 0 (apply c-dims nn)) (lambda () (random n))))
                   (sra1 (call-with-output-string (cut (@@ (newra print) ra-print) ra <> #:dims? #t)))
                   (sra2 (call-with-output-string (cut (@@ (newra print) ra-print) ra <> #:dims? #f)))
                   (a (ra->array ra))
                   (sa (call-with-output-string (cut display a <>))))
              (format #t "rank ~a ~a:" rank nn)
              (let ((rb #f) (b #f))
                (format-line (* scale (time (set! rb (call-with-input-string sra1 read))))
                             (* scale (time (set! rb (call-with-input-string sra2 read))))
                             (* scale (time (set! b (call-with-input-string sa read)))))
                (unless (array-equal? (ra->array rb) b) (throw 'bad-reading-benchmark)))))
        (iota 6 1)))
  (list #t 'f64))

(define m #e1e5)
(format #t "\nlist->ra\n==================\n")
(for-each
    (lambda (type)
      (format #t "\ntype dst ~a\n----------" type)
      (format-header "ra" "array" "ra/shape" "array/shape")
      (for-each
          (lambda (rank)
            (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
                   (nn (make-list rank n))
                   (len (fold * 1 nn))
                   (scale (* 1e3 (/ m len)))
                   (ra (ra-map! (make-ra-new type 0 (apply c-dims nn)) (lambda () (random n))))
                   (la (ra->list ra))
                   (shape (map (lambda (len) (list 0 (- len 1))) nn)))
              (format #t "rank ~a ~a:" rank nn)
              (let ((rb #f) (b #f))
                (format-line (* scale (time (set! rb (list->ra rank la))))
                             (* scale (time (set! b (list->array rank la))))
                             (* scale (time (set! rb (list->typed-ra #t shape la))))
                             (* scale (time (set! b (list->typed-array #t shape la)))))
                (unless (array-equal? (ra->array rb) b) (throw 'bad-ra->list-benchmark))
                (unless (array-equal? (ra->array rb) b) (throw 'bad-ra->list-benchmark)))))
        (iota 6 1)))
  (list #t 'f64))


; -----------------------
; ra-index-map!
; -----------------------

(define m #e1e5)
(format #t "\nra-index-map!\n==================\n")
(for-each
    (lambda (type)
      (format #t "\ntype dst ~a\n----------" type)
      (format-header "ra" "array")
      (for-each
          (lambda (rank)
            (let* ((op (lambda x (apply + x)))
                   (n (inexact->exact (ceiling (expt m (/ rank)))))
                   (nn (make-list rank n))
                   (len (fold * 1 nn))
                   (scale (* 1e3 (/ m len)))
                   (ra (ra-map! (make-ra-new type 0 (apply c-dims nn)) (lambda () (random n))))
                   (array (ra->array ra)))
              (format #t "rank ~a ~a:" rank nn)
              (format-line (* scale (time (ra-index-map! ra op)))
                           (* scale (time (array-index-map! array op))))
              (unless (array-equal? (ra->array ra) array) (throw 'bad-ra-index-map!-benchmark))))
        (iota 3 1)))
  (list #t 'f64))


; -----------------------
; some profiling...
; -----------------------

(import (statprof))

(format #t "\nstatprof...\n==================\n")
(define m #e5e4)
(define type #t)
(let* ((rank 3)
       (n (inexact->exact (ceiling (expt (* 10 m) (/ rank)))))
       (nn (make-list rank n))
       (ra0 (make-ra-new type *unspecified* (apply c-dims nn)))
       (ra1 (ra-map! (make-ra-new type 0 (apply c-dims nn)) (lambda () (random n))))
       (ra2 (ra-map! (make-ra-new type 0 (apply c-dims nn)) (lambda () (random n))))
       (s (call-with-output-string (cut display ra1 <>)))
       (prof (lambda () (call-with-input-string s read)))
       (prof (lambda () (ra-fill! ra0 99)))
       (prof (lambda () (ra-copy! ra1 ra2)))
       (prof (lambda () (ra-map! ra0 * ra1 ra2)))
       )
  (statprof prof #:count-calls? #t)
  prof)
