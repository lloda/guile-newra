; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2018-2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Library for Newra - hub, plus old arrays compatiblity & misc.
;;; Code:

(define-module (newra lib)
  #:export (ra-index-map!
            make-ra make-typed-ra make-ra-shared ra->list
            array->ra ra->array as-ra
            ra-i ra-iota
            ra-copy
            ra-fold ra-fold*))

(import (newra base) (newra map) (only (srfi :1) fold every any iota drop) (srfi :8) (srfi :26)
        (ice-9 control) (ice-9 match) (only (rnrs base) vector-map vector-for-each)
        (newra lib reshape))

(re-export ra-reverse ra-transpose ra-untranspose ra-order-c?
           ra-ravel ra-reshape ra-tile
           ra-singletonize)


; ----------------
; fold - probably better ways to do this by fixing or extending (newra map)
; ----------------

(define-inlinable-case ra-fold
  (case-lambda
   "
ra-fold kons knil ra ...

Reduce ra RA by (... (KONS RA1 ... (KONS RA0 ... KNIL)) ...) where
(RA0 ...), (RA1 ...) is some sequence of the elements of RA ....

See also: ra-fold* ra-map! ra-for-each ra-slice-for-each
"
   ((op init)
    init)
   ((op init a0)
    (ra-for-each (lambda (x0) (set! init (op x0 init))) a0)
    init)
   ((op init a0 a1)
    (ra-for-each (lambda (x0 x1) (set! init (op x0 x1 init))) a0 a1)
    init)
   ((op init a0 a1 a2)
    (ra-for-each (lambda (x0 x1 x2) (set! init (op x0 x1 x2 init))) a0 a1 a2)
    init)
   ((op init . args)
    (apply ra-for-each (lambda x (set! init (apply op (append! x (list init))))) args)
    init)))

; really prefer this order, possibly get rid of (ra-fold).
(define-inlinable-case ra-fold*
  (case-lambda
   "
ra-fold* kons knil ra ...

Reduce ra RA by (... (KONS (KONS KNIL RA0 ...) RA1 ... ) ...) where
(RA0 ...), (RA1 ...) is some sequence of the elements of RA ....

See also: ra-fold ra-map! ra-for-each ra-slice-for-each
"
   ((op init)
    init)
   ((op init a0)
    (ra-for-each (lambda (x0) (set! init (op init x0))) a0)
    init)
   ((op init a0 a1)
    (ra-for-each (lambda (x0 x1) (set! init (op init x0 x1))) a0 a1)
    init)
   ((op init a0 a1 a2)
    (ra-for-each (lambda (x0 x1 x2) (set! init (op init x0 x1 x2))) a0 a1 a2)
    init)
   ((op init . args)
    (apply ra-for-each (lambda x (set! init (apply op init x))) args)
    init)))


; ----------------
; transition help
; ----------------

(define (array->ra a)
  (let ((dims (list->vector
               (map (lambda (b i) (make-dim (- (cadr b) (car b) -1) (car b) i))
                 (array-shape a)
                 (shared-array-increments a)))))
    (make-ra-root (shared-array-root a) dims
                  (- (shared-array-offset a) (ra-offset 0 dims)))))

(define (ra->array ra)
  (when (eq? 'd (ra-type ra))
    (throw 'nonconvertible-type (ra-type ra)))
  (apply make-shared-array (ra-root ra)
         (lambda i (list (apply ra-pos (ra-zero ra) (ra-dims ra) i)))
         (vector->list (vector-map (lambda (dim) (list (dim-lo dim) (dim-hi dim)))
                                   (ra-dims ra)))))

(define (make-typed-ra type value . d)
  "
make-typed-ra type value d ...

Return a new ra of type TYPE and shape D. The ra will be initialized with
VALUE. VALUE may be *unspecified*, in which case the ra may be left
uninitialized.

See also: make-ra
"
  (make-ra-new type value (apply c-dims d)))

(define (make-ra value . d)
  "
make-ra value d ...

Equivalent to (make-typed-ra #t value d ...).

See also: make-typed-ra
"
  (make-ra-new #t value (apply c-dims d)))

(define (make-ra-shared oldra mapfunc . d)
  "
make-ra-shared oldra mapfunc ... d

Return a shared array with shape D over the root of OLDRA. MAPFUNC is a function
of the indices of the new array and must return a list of indices to
OLDRA.

MAPFUNC is only called the minimum (+ 1 (length D)) times that are needed in
order to determine an affine function from one set of indices to the other.

For example, to displace the bounds of an array without affecting its contents:

guile> (define x (ra-i 3 2))
guile> x
#%2d:3:2((0 1) (2 3) (4 5))
guile> (make-ra-shared x (lambda (i j) (list (- i 2) (+ j 3))) '(2 4) '(-3 -2))
#%2d@2:3@-3:2((0 1) (2 3) (4 5))

See also: make-ra, FIXME ra-affine-map
"
; get lo & len, won't use step except if the result is empty.
  (let* ((oldra (ra-check oldra))
         (dims (apply c-dims d))
         (newrank (vector-length dims)))
; if the result *is* empty then it has no valid indices, so we cannot call mapfunc.
    (let emptycheck ((k 0))
      (if (< k newrank)
        (if (positive? (dim-len (vector-ref dims k)))
          (emptycheck (+ 1 k))
          (make-ra-root (%%ra-root oldra) dims (%%ra-zero oldra)))
        (let* ((los (vector->list (vector-map dim-lo dims)))
               (ref (apply ra-pos (%%ra-zero oldra) (%%ra-dims oldra) (apply mapfunc los)))
               (dims (vector-map
                      (lambda (dim step) (make-dim (dim-len dim) (dim-lo dim) step))
                      dims
                      (let ((steps (make-vector newrank 0)))
                        (let loop ((k 0))
                          (cond
                           ((= k newrank) steps)
                           (else
                            (vector-set!
                             steps k
                             (if (positive? (dim-len (vector-ref dims k)))
                               (let ((ii (list-copy los)))
                                 (list-set! ii k (+ 1 (list-ref los k)))
                                 (- (apply ra-pos (%%ra-zero oldra) (%%ra-dims oldra) (apply mapfunc ii)) ref))
                               0))
                            (loop (+ k 1)))))))))
          (make-ra-root (%%ra-root oldra) dims (- ref (ra-offset 0 dims))))))))

; FIXME Depends on traversal order of ra-for-each.

(define (ra->list ra)
  "
ra->list ra

Return a nested list of the elements of ra RA. For example, if RA is a 1-rank
ra, the list contains the elements of RA; if RA is a 2-rank ra, the list
contains a list for each of the rows of RA; and so on.

See also: as-ra
"
  (let ((ra (ra-check ra))
        (rank (%%ra-rank ra)))
    (cond
     ((zero? rank) (ra-ref ra))
     (else
      (let ((ra (apply ra-reverse ra (iota rank)))
            (dimk (vector-ref (%%ra-dims ra) (- rank 1))))
        (let loop-rank ((ra ra))
          (cond
           ((= 1 (%%ra-rank ra))
            (if (> (dim-len dimk) 20)
              (ra-fold cons '() ra)
              (let loop-dim ((l '()) (i (dim-lo dimk)))
                (if (> i (dim-hi dimk))
                  l
                  (loop-dim (cons (ra-ref ra i) l) (+ i 1))))))
           (else
            (let ((l '()))
              (ra-slice-for-each 1 (lambda (x) (set! l (cons (loop-rank x) l))) ra)
              l)))))))))

; Similar to (@ (newra newra) ra-for-each-slice-1) - since we cannot unroll. It
; might be cheaper to go Fortran order (building the index lists back to front);
; should try that. C order and set-cdr! is how oldra does it.
; This function is provided for compatibility with oldra; generally we shouldn't
; be building index lists.

(define (ra-index-map! ra op)
  "
ra-index-map! ra op -> ra

Apply OP to the indices of each element of RA in turn, storing the result in the
corresponding element.  The value returned and the order of application are
unspecified.

This function returns the modified RA.

For example:

guile> (define x (make-ra 0 2 3))
guile> (ra-index-map! x (lambda x x))
guile> x
#%2:2:3(((0 0) (0 1) (0 2)) ((1 0) (1 1) (1 2)))

See also: ra-iota ra-i
"
  (let* ((kk (ra-rank ra))
         (ii (make-list kk)))
    (receive (los lens) ((@ (newra map) ra-slice-for-each-check) kk ra)
      (if (= kk 0)
        (ra-set! ra (apply op ii))
        (let loop-rank ((k 0) (ra ra) (endi ii))
          (let* ((lo (vector-ref los k))
                 (end (+ lo (vector-ref lens k))))
            (if (= (+ 1 k) kk)
              (let loop-dim ((i lo))
                (if (= i end)
                  (set-car! endi lo)
                  (begin
                    (set-car! endi i)
                    (ra-set! ra (apply op ii) i)
                    (loop-dim (+ i 1)))))
              (let loop-dim ((i lo))
                (if (= i end)
                  (set-car! endi lo)
                  (begin
                    (set-car! endi i)
                    (loop-rank (+ k 1) (ra-slice ra i) (cdr endi))
                    (loop-dim (+ i 1))))))))))
    ra))


; ----------------
; functions not found in oldra, or significantly extended
; ----------------

; FIXME partial implementation.

(define* (as-ra ra #:key (type (ra-type ra)) (new? #f))
  (cond ((and (eq? (ra-type ra) type) (not new?)) ra)
        (else (ra-copy! (make-ra-new
                         type *unspecified*
                         (apply c-dims (map dim-len (vector->list (ra-dims ra)))))
                        ra))))

(define (ra-i . i)
  (make-ra-root (make-aseq) (apply c-dims i)))

(define ra-iota
  (case-lambda*
   (()
; lo #f so it matches axes with any lo
    (make-ra-root (make-aseq) (vector (make-dim #f #f 1))))
   ((len #:optional (lo 0) (step 1))
    (make-ra-root (make-aseq lo step) (c-dims len)))))

; oldra has array-copy in (ice-9 arrays). Something of the sort.
; FIXME handling of dead axes could be faster - maybe c-dims should be written differently.

(define ra-copy
  (case-lambda
   "
ra-copy ra -> rb
ra-copy type ra -> rb

Copy the contents of RA into a new ra RB of type TYPE and the same shape as
RA. TYPE defaults to (ra-type RA) if not given.

If RA has dead axes, those are preserved in RB.

See also: ra-copy! as-ra
"
   ((ra) (ra-copy (ra-type ra) ra))
   ((type ra)
    (let* ((shape (map (match-lambda
                         (($ <dim> len lo step)
                          (let ((lo (or lo 0)))
                            (list lo
                                  (if len
                                    (+ lo len -1)
                                    (if (zero? step)
                                      lo
                                      (throw 'cannot-copy-infinite-ra ra)))))))
                    (vector->list (ra-dims ra))))
; copy destination needs the singletons else the lens don't match.
; FIXME we could have dead axes match dead axes.
           (rb (ra-copy! (make-ra-new type *unspecified* (apply c-dims shape)) ra)))
; preserve dead axes in the result.
      (if (any not (ra-dimensions ra))
        (make-ra-root (ra-root rb)
                      (vector-map (lambda (a b) (if (dim-len a) b a)) (ra-dims ra) (ra-dims rb))
                      (ra-zero rb))
        rb)))))
