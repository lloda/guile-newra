; -*- mode: scheme; coding: utf-8 -*-

;; (c) Daniel Llorens - 2020-2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Concatenation procedures for Newra.
;;; Code:

(define-module (newra cat)
  #:export (ra-pcat ra-scat))

(import (srfi :1) (srfi :26) (newra base) (newra lib) (newra from) (ice-9 match))

(define (list-subst! l k val)
  (list-set! l k val)
  l)

(define (plain-cat! i dest . xx)
  (fold (lambda (x base)
          (let ((dc (if (< i (ra-rank x)) (ra-len x i) 1)))
            (ra-amend! dest x (ldots i) (ra-iota dc base))
            (+ base dc)))
        0 xx)
  dest)

; FIXME allow inf lens in non-concatenating axes?

(define (ra-pcat type i . xx)
  "
Concatenate arrays @var{xx} ... along axis @var{i}. The shapes of @var{xx} ... must
have matching prefixes except at axis @var{i}.

The output array will have the rank of the @var{xx} with the largest rank, or
@code{(+ 1 i)}, whichever is larger. If necessary, the @var{xx} are rank
extended to this output rank. The lengths of @var{xx} must match on all axes
other than @var{i}.

If @var{i} is negative, the shape of each @var{xx} ... is prefix-extended by
@code{(- i)} singleton dimensions and the concatenation is carried out along the
first axis.

@code{ra-pcat} always creates a new array and not a shared array.

The type of the output is @var{type}, unless @code{#f}; else the type of the
first argument, unless @code{'d}; else @code{#t}.

'pcat' stands for 'prefix-cat'.

For example:

@verbatim
(ra-pcat #t 0 (ra-i 1 2) (ra-i 2 2))         => #%2((0 1) (0 1) (2 3)))
(ra-pcat #t 0 (ra-iota 2 1) (ra-iota 3 3))   => #%1(1 2 3 4 5))
(ra-pcat #t -1 (ra-iota 2 1) (ra-iota 2 4))  => #%2((1 2) (4 5))
(ra-pcat #t 1 (ra-iota 2 1) (ra-iota 2 4))   => #%2((1 4) (2 5))
(ra-pcat #t 0 (make-ra 'a) (ra-iota 2))      => #%1(a 0 1)
(ra-pcat #t 1 (make-ra 'a) (ra-iota 2))      => #%2((a 0) (a 1))
(ra-pcat #t -1 (make-ra 'a) (ra-iota 2))     => #%2((a a) (0 1))
(ra-pcat #t 1 (array->ra #(a b)) (ra-i 2 2)) => #%2((a 0 1) (b 2 3))
(ra-pcat #t 0 (array->ra #(a b)) (ra-i 2 2)) => #%2((a a) (b b) (0 1) (2 3))
@end verbatim

See also: ra-scat ra-tile ra-tile-right
"
  (if (> 0 i)
    (apply ra-pcat type 0 (map (cute apply ra-tile <> (make-list (max 0 (- i)) 1)) xx))
    (match xx
      (()
       (throw 'ra-pcat-missing-arguments))
      (xx
       (let ((xm (fold (lambda (x xm) (if (> (ra-rank x) (ra-rank xm)) x xm)) (car xx) (cdr xx))))
         (apply plain-cat! i
                (apply make-typed-ra
                  (or type (match (ra-type (car xx))  ('d #t) (t t)))
                  *unspecified*
                  (list-subst! (append (ra-dimensions xm) (make-list (max 0 (- (+ 1 i) (ra-rank xm))) 1))
                               i (fold (lambda (x o) (+ o (if (> (ra-rank x) i) (ra-len x i) 1))) 0 xx)))
                xx))))))

(define (ra-scat type i . xx)
  "
Concatenate items of rank @var{i} of arrays @var{xx} ... The shapes of @var{xx}
... must have matching suffixes except at axis @code{(- (ra-rank x) 1 i)} for
each @var{x} in @var{xx}.

The output array will have the rank of the @var{xx} with the largest rank, or
@code{(+ 1 i)}, whichever is larger. If necessary, the @var{xx} are rank
extended to this output rank.

If @var{i} is negative, the shape of each array @var{xx} ... is suffix-extended
by @code{(- i)} singleton dimensions and the concatenation is carried out along
the last axis.

@code{ra-scat} always creates a new array and not a shared array.

The type of the output is @var{type}, unless @code{#f}; else the type of the
first argument, unless @code{'d}; else @code{#t}.

'scat' stands for 'suffix-cat'.

For example:

@verbatim
(ra-scat #t 0 (make-ra 'a) (make-ra 'b) (make-ra 'c))              => #%1(a b c)
(ra-scat #t 1 (make-ra 'a) (make-ra 'b) (make-ra 'c))              => #%2((a) (b) (c))
(ra-scat #t 0 (array->ra #(1 2 3)) (make-ra 4) (array->ra #(5 6))) => #%1(1 2 3 4 5 6)
(ra-scat #t 0 (array->ra #2((0 1) (2 3))) (array->ra #(a b)))      => #%2((0 1 a b) (2 3 a b)))
(ra-scat #t 1 (array->ra #2((0 1) (2 3))) (array->ra #(a b)))      => #%2((0 1) (2 3) (a b))
(ra-scat #t 1 (array->ra #2((0 1))) (array->ra #(a)))              => error, mismatched dimensions
(ra-scat #t 0 (array->ra #2((0 1))) (array->ra #(a)))              => #%2((0 1 a))
(ra-scat #t -1 (array->ra #(1 2 3)) (array->ra #(a b c)))          => #%2((1 a) (2 b) (3 c))
(ra-scat #t -1 (make-ra 'a) (array->ra #(x y z)))                  => #%2((a x) (a y) (a z))
@end verbatim

See also: ra-pcat ra-tile ra-tile-right
"
  (if (> 0 i)
    (apply ra-scat type 0 (map (cute apply ra-tile-right <> (make-list (max 0 (- i)) 1)) xx))
    (match xx
      (()
       (throw 'ra-scat-missing-arguments))
      (xx
       (let* ((xm (fold (lambda (x xm) (if (> (ra-rank x) (ra-rank xm)) x xm)) (car xx) (cdr xx)))
              (im (max (+ 1 i) (ra-rank xm)))
              (ii (- im 1 i))
              (xx (map (lambda (x)
                         (let ((ext (append (make-list (- im (ra-rank xm)) 1)
                                            (take (ra-dimensions xm) (- (ra-rank xm) (ra-rank x))))))
                           (apply ra-tile x (if (> (ra-rank x) i)
                                              ext
                                              (list-subst! ext ii 1)))))

                    xx)))
         (apply plain-cat! ii
                (apply make-typed-ra
                  (or type (match (ra-type (car xx))  ('d #t) (t t)))
                  *unspecified*
                  (list-subst! (ra-dimensions (car xx))
                               ii (fold (lambda (x o) (+ o (ra-len x ii))) 0 xx)))
                xx))))))
