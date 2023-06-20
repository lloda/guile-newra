; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2018-2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Library for newra - hub, plus old arrays compatiblity & misc.
;;; Code:

(define-module (newra lib)
  #:export (ra-index-map!
            make-ra make-typed-ra make-ra-shared ra->list
            array->ra ra->array as-ra
            ra-i ra-iota
            ra-copy ra-map
            ra-fold))

(import (only (srfi srfi-1) fold every any iota drop xcons) (srfi srfi-26) (srfi srfi-71)
        (ice-9 control) (ice-9 match) (only (rnrs base) vector-map vector-for-each)
        (newra base) (newra map) (newra reshape))

(define (ra-unique-elements? ra)
  "
Return @code{#t} if every combination of valid indices for the array @var{ra} yields a
different element in @var{ra}'s root, otherwise return @code{#f}.
"
  (throw 'not-implemented))


; ----------------
; fold - probably better ways to do this by fixing or extending (newra map)
; ----------------

(define-inlinable-case ra-fold
  (case-lambda
   "
Reduce arrays @var{a} by (... (@var{kons} (@var{cons} @var{knil} @var{a}0 ...)
@var{a}1 ... ) ...) where (@var{a}0 ...), (@var{a}1 ...) is the row-major ravel
of @var{a}...

See also: @code{ra-fold ra-map!} @code{ra-for-each} @code{ra-slice-for-each}
"
   ((kons knil)
    knil)
   ((kons knil a0)
    (ra-for-each (lambda (x0) (set! knil (kons knil x0))) a0)
    knil)
   ((kons knil a0 a1)
    (ra-for-each (lambda (x0 x1) (set! knil (kons knil x0 x1))) a0 a1)
    knil)
   ((kons knil a0 a1 a2)
    (ra-for-each (lambda (x0 x1 x2) (set! knil (kons knil x0 x1 x2))) a0 a1 a2)
    knil)
   ((kons knil . args)
    (apply ra-for-each (lambda x (set! knil (apply kons knil x))) args)
    knil)))


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
         (vector->list (vector-map (match-lambda (($ <dim> len lo _) (list lo (dim-hi len lo))))
                                   (ra-dims ra)))))

(define (make-typed-ra type value . d)
  "
Return a new array of type @var{type} and shape @var{d}. The array will be
initialized with @var{value}. @var{value} may be *unspecified*, in which case
the array contents may be left uninitialized.

See also: @code{make-ra}
"
  (make-ra-new type value (apply c-dims d)))

(define (make-ra value . d)
  "
Equivalent to @code{(make-typed-ra #t value d ...)}.

See also: @code{make-typed-ra}
"
  (make-ra-new #t value (apply c-dims d)))

(define (make-ra-shared oldra mapf . d)
  "
Return a shared array with shape @var{d} over the root of @var{oldra}. @var{mapf}
is a function of the indices of the new array and must return a list of indices
to @var{oldra}.

@var{mapf} is only called the minimum (+ 1 (length d)) times that
are needed in order to determine an affine function from one set of indices to
the other.

For example, to displace the bounds of an array without affecting its contents:

@verbatim
guile> (define x (ra-i 3 2))
guile> x
#%2d:3:2((0 1) (2 3) (4 5))
guile> (make-ra-shared x (lambda (i j) (list (- i 2) (+ j 3))) '(2 4) '(-3 -2))
#%2d@2:3@-3:2((0 1) (2 3) (4 5)
@end verbatim

See also: @code{make-ra}
"
; get lo & len, won't use step except if the result is empty.
  (let* ((oldra (ra-check oldra))
         (dims (apply c-dims d))
         (newrank (vector-length dims)))
; if the result *is* empty then it has no valid indices, so we cannot call mapf.
    (let emptycheck ((k 0))
      (if (< k newrank)
        (if (positive? (dim-len (vector-ref dims k)))
          (emptycheck (+ 1 k))
          (make-ra-root (%%ra-root oldra) dims (%%ra-zero oldra)))
        (let* ((los (vector->list (vector-map dim-lo dims)))
               (ref (apply ra-pos (%%ra-zero oldra) (%%ra-dims oldra) (apply mapf los)))
               (dims (vector-map
                      (match-lambda* ((($ <dim> len lo _) step) (make-dim len lo step)))
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
                                 (- (apply ra-pos (%%ra-zero oldra) (%%ra-dims oldra) (apply mapf ii)) ref))
                               0))
                            (loop (+ k 1)))))))))
          (make-ra-root (%%ra-root oldra) dims (- ref (ra-offset 0 dims))))))))

; FIXME Depends on traversal order of ra-for-each.

(define (ra->list ra)
  "
Return a nested list of the elements of array @var{ra}. For example, if @var{ra} is a
1-rank array, the list contains the elements of @var{ra}; if @var{ra} is a 2-rank ra, the
list contains a list for each of the rows of @var{ra}; and so on.

See also: @code{as-ra}
"
  (let* ((ra (ra-check ra))
         (rank (%%ra-rank ra)))
    (cond
     ((zero? rank) (ra-ref ra))
     (else
      (let ((ra (apply ra-reverse ra (iota rank))))
        (match (vector-ref (%%ra-dims ra) (- rank 1))
          (($ <dim> klen klo kstep)
           (let loop-rank ((ra ra))
             (cond
              ((= 1 (%%ra-rank ra))
               (if (> klen 20)
                 (ra-fold xcons '() ra)
                 (let loop-dim ((l '()) (i klo))
                   (if (> i (dim-hi klen klo))
                     l
                     (loop-dim (cons (ra-ref ra i) l) (+ i 1))))))
              (else
               (let ((l '()))
                 (ra-slice-for-each 1 (lambda (x) (set! l (cons (loop-rank x) l))) ra)
                 l)))))))))))

; Similar to (@ (newra) ra-for-each-slice-1) - since we cannot unroll. It
; might be cheaper to go Fortran order (building the index lists back to front);
; should try that. C order and set-cdr! is how oldra does it.
; This function is provided for compatibility with oldra; generally we shouldn't
; be building index lists.

(define (ra-index-map! ra op)
  "
Apply @var{op} to the indices of each element of array @var{ra} in turn, storing the
result in the corresponding element. The order of iteration is unspecified.

This function returns the modified @var{ra}.

For example:

@example
(define x (make-ra 0 2 3))
(ra-index-map! x (lambda x x))
x @result{} #%2:2:3(((0 0) (0 1) (0 2)) ((1 0) (1 1) (1 2)))
@end example

See also: @code{ra-iota} @code{ra-i}
"
  (let* ((kk (ra-rank ra))
         (ii (make-list kk))
         (los lens ((@ (newra map) ra-slice-for-each-check) kk ra)))
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
                    (loop-dim (+ i 1)))))))))
      ra))


; ----------------
; functions not found in oldra, or significantly extended
; ----------------

; FIXME partial implementation.

(define* (as-ra ra #:key type new? rank)
  (cond ((and (eq? (ra-type ra) type) (not new?)) ra)
        (else (ra-copy! (make-ra-new
                         type *unspecified*
                         (apply c-dims (map dim-len (vector->list (ra-dims ra)))))
                        ra))))

(define (ra-i . i)
  (make-ra-root (make-aseq) (apply c-dims i)))

; FIXME (ra-iota #f ...) is somewhat inconsistent with (ra-i #t). Maybe accept ra-iota #t instead?
(define ra-iota
  (case-lambda*
   (()
; lo #f so it matches axes with any lo
    (make-ra-root (make-aseq) (vector (make-dim #f #f 1))))
   ((len #:optional (lo 0) (step 1))
    (make-ra-root (make-aseq lo step) (vector (make-dim len 0 1))))))

; oldra has array-copy in (ice-9 arrays). Something of the sort.
; FIXME handling of dead axes could be faster - maybe c-dims should be written differently.

(define ra-copy
  (case-lambda
   "
@deffn @w{function} ra-copy src
@deffnx @w{function} ra-copy type src

Copy the contents of @var{src} into a new ra of type @var{type} and the same
shape as @var{src}. @var{type} defaults to @code{(ra-type src)} if not given, or
@code{#t} if @code{(ra-type src)} is @code{'d}.

If @var{src} has dead axes, those are preserved in the result.

See also: @code{ra-copy!} @copy{as-ra}
"
   ((ra) (ra-copy #f ra))
   ((type ra)
    (let* ((type (or type (match (ra-type ra) ('d #t) (t t))))
           (shape (map (match-lambda
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

(define (ra-map type op rx0 . rx)
  "
Same as @code{(ra-map! ra rx0 rx ...)}, except that the result is created from
@var{type} and the shapes of @var{rx} ...

If @var{type} is #f, then the type of the result is @code{(ra-type rx0)}, or
@code{#t} if @code{(ra-type rx0)} is @code{'d}.

See also: @code{ra-map!}
"
  (let* ((k (fold (lambda (a b) (max b (ra-rank a))) (ra-rank rx0) rx))
; FIXME vector->list is ugly. Need to refine/formalize the 'arguments match' routine
         (lo len (apply ra-slice-for-each-check k rx0 rx)))
    (apply ra-map! (apply make-typed-ra (or type (match (ra-type rx0) ('d #t) (t t)))
                          *unspecified*
                          (map (lambda (lo len) (list lo (+ lo len -1)))
                            (vector->list lo) (vector->list len)))
           op rx0 rx)))
