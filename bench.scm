
; Replacement for Guile C-based array system - Benchmarks
; (c) Daniel Llorens - 2016-2017
; Run with $GUILE -L mod -s bench.scm

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(import (newra newra) (newra print) (newra tools) (newra test) (newra read)
        (only (rnrs base) vector-map)
        (srfi srfi-26) (srfi srfi-8) (only (srfi srfi-1) fold iota)
        (ice-9 match) (ice-9 format))


; -----------------------
; benchmarks
; -----------------------

(define-syntax %ra-loop-inner
  (lambda (stx-inner)
    (syntax-case stx-inner ()
      ((_ lens k u body nn ...)
       (let ((uu (syntax->datum #'u)))
         (if (= uu (syntax->datum #'k))
           #'body
           (with-syntax ((nu (list-ref #'(nn ...) uu)))
             #`(let ((end (vector-ref lens u)))
                 (let loop ((nu 0))
                   (unless (= nu end)
                     (%ra-loop-inner lens k #,(+ uu 1) body nn ...)
                     (loop (+ nu 1))))))))))))

(define-syntax %ra-loop
  (lambda (stx)
    (syntax-case stx ()
      ((_ lens k (i ...) body)
       #'(begin
           (unless (= (vector-length lens) k) (throw 'bad-rank))
           (%ra-loop-inner lens k 0 body i ...))))))

(define (loop-fun dims f)
  (case (vector-length dims)
    ((0) (%ra-loop dims 0 () (f)))
    ((1) (%ra-loop dims 1 (i) (f i)))
    ((2) (%ra-loop dims 2 (i j) (f i j)))
    ((3) (%ra-loop dims 3 (i j k) (f i j k)))
    ((4) (%ra-loop dims 4 (i j k l) (f i j k l)))
    (else (throw 'not-implemented))))

(define (ra-loop ra f)
  (loop-fun (vector-map dim-len (ra-dims ra)) f))

(define (array-loop a f)
  (loop-fun (list->vector (array-dimensions a)) f))

(define ra0 (make-ra-data (make-dim 1) (c-dims)))
(define ra1 (make-ra-data (make-dim (* 2)) (c-dims 2)))
(define ra2 (make-ra-data (make-dim (* 2 3)) (c-dims 2 3)))
(define ra3 (make-ra-data (make-dim (* 2 3 4)) (c-dims 2 3 4)))

(%ra-loop (vector-map dim-len (ra-dims ra0)) 0 () (display (ra0)))
(%ra-loop (vector-map dim-len (ra-dims ra1)) 1 (i) (display (+ i i)))
(%ra-loop (vector-map dim-len (ra-dims ra2)) 2 (i j) (display (+ i j)))
(%ra-loop (vector-map dim-len (ra-dims ra3)) 3 (i j k) (display (+ i j k)))

(define a0 (ra->array (as-ra ra0 #:type #t)))
(define a1 (ra->array (as-ra ra1 #:type #t)))
(define a2 (ra->array (as-ra ra2 #:type #t)))
(define a3 (ra->array (as-ra ra3 #:type #t)))

(ra-loop ra0 (lambda () (display (ra0))))
(ra-loop ra1 (lambda (i) (display (ra1 i))))
(ra-loop ra2 (lambda (i j) (display (ra2 i j))))
(ra-loop ra3 (lambda (i j k) (display (ra3 i j k))))

(array-loop a0 (lambda () (display (array-ref a0))))
(array-loop a1 (lambda (i) (display (array-ref a1 i))))
(array-loop a2 (lambda (i j) (display (array-ref a2 i j))))
(array-loop a3 (lambda (i j k) (display (array-ref a3 i j k))))

(define m #e1e5)
(format #t "\nra-cell (applicable function) / array-ref\n==========\n")
(for-each
 (lambda (type)
   (format #t "\ntype ~a\n---------\n" type)
   (for-each
    (lambda (rank)
      (let* ((n (inexact->exact (ceiling (expt m (/ rank)))))
             (nn (make-list rank n))
             (len (fold * 1 nn))
             (scale (* 1e3 (/ m len)))
             (ra (as-ra (make-ra-data (make-dim len) (apply c-dims nn)) #:type #t))
             (a (ra->array ra))
             (ras 0)
             (as 0)
; FIXME test ra-ref / ra-cell / (ra ...)
             (raf-cell (case-lambda ((i) (set! ras (+ ras (ra-ref ra i))))
                                    ((i j) (set! ras (+ ras (ra-ref ra i j))))
                                    ((i j k) (set! ras (+ ras (ra-ref ra i j k))))
                                    ((i j k l) (set! ras (+ ras (ra-ref ra i j k l))))))
             (raf-ref (case-lambda ((i) (set! ras (+ ras (ra-cell ra i))))
                                   ((i j) (set! ras (+ ras (ra-cell ra i j))))
                                   ((i j k) (set! ras (+ ras (ra-cell ra i j k))))
                                   ((i j k l) (set! ras (+ ras (ra-cell ra i j k l))))))
             (raf-appl (case-lambda ((i) (set! ras (+ ras (ra i))))
                                    ((i j) (set! ras (+ ras (ra i j))))
                                    ((i j k) (set! ras (+ ras (ra i j k))))
                                    ((i j k l) (set! ras (+ ras (ra i j k l))))))
             (af (case-lambda ((i) (set! as (+ as (array-ref a i))))
                              ((i j) (set! as (+ as (array-ref a i j))))
                              ((i j k) (set! as (+ ras (array-ref a i j k))))
                              ((i j k l) (set! as (+ ras (array-ref a i j k l)))))))
        (unless (= ras as) (throw 'error-in-ra-cell-array-ref-check))
        (format #t "rank ~a (nn ~a)\n" rank nn)
        (format #t "ra-cell~20t~9,4f\n" (* scale (time (ra-loop ra raf-cell))))
        (format #t "ra-ref~20t~9,4f\n" (* scale (time (ra-loop ra raf-ref))))
        (format #t "ra-appl~20t~9,4f\n" (* scale (time (ra-loop ra raf-appl))))
        (format #t "array~20t~9,4f\n" (* scale (time (array-loop a af))))))
    (iota 4 1)))
 '(#t f64))

(define m #e1e5)
(format #t "\nra-slice-for-each array-slice-for-each ra-map! array-map!\n==========\n")
(for-each
 (lambda (type)
   (for-each
    (lambda (nargs)
      (format #t "\ntype ~a ~a args\n---------\n" type nargs)
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
           (format #t "rank ~a (nn ~a)\n" rank nn)
           (case nargs
             ((3)
              (format #t "ra*~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-4 ra20 - ra21 ra22))))
              (format #t "array*~20t~9,4f\n" (* scale (time (array-map*! a20 - a21 a22))))
              (format #t "ra~20t~9,4f\n" (* scale (time (ra-map! ra20 - ra21 ra22))))
              (format #t "array~20t~9,4f\n" (* scale (time (array-map! a20 - a21 a22)))))
             ((2)
              (format #t "ra*~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-4 ra20 - ra21))))
              (format #t "array*~20t~9,4f\n" (* scale (time (array-map*! a20 - a21))))
              (format #t "ra~20t~9,4f\n" (* scale (time (ra-map! ra20 - ra21))))
              (format #t "array~20t~9,4f\n" (* scale (time (array-map! a20 - a21)))))
             ((1)
              (format #t "ra*~20t~9,4f\n" (* scale (time (ra-map*! ra-slice-for-each-4 ra20 (lambda () (random n))))))
              (format #t "array*~20t~9,4f\n" (* scale (time (array-map*! a20 (lambda () (random n))))))
              (format #t "ra~20t~9,4f\n" (* scale (time (ra-map! ra20 (lambda () (random n))))))
              (format #t "array~20t~9,4f\n" (* scale (time (array-map! a20 (lambda () (random n))))))))))
       (iota 6 1)))
    (iota 3 1)))
 '(#t f64))

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
               (ra20 (make-ra-new typesrc *unspecified* (apply c-dims nn)))
               (ra21 (ra-map! (make-ra-new typedst 0 (apply c-dims nn)) (lambda () (random n))))
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
               (ra20 (make-ra-new type *unspecified* (apply c-dims nn)))
               (a20 (ra->array ra20)))
          (format #t "rank ~a (nn ~a)\n" rank nn)
          (format #t "ra~20t~9,4f\n" (* scale (time (ra-fill! ra20 77))))
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
               (ra20 (ra-map! (make-ra-new type 0 (apply c-dims nn)) (lambda () (random n))))
               (ra21 (ra-copy! ra20 (make-ra-new type 0 (apply c-dims nn))))
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
               (ra (ra-map! (make-ra-new type 0 (apply c-dims nn)) (lambda () (random n))))
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
               (ra (ra-map! (make-ra-new type 0 (apply c-dims nn)) (lambda () (random n))))
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
               (ra (ra-map! (make-ra-new type 0 (apply c-dims nn)) (lambda () (random n))))
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
