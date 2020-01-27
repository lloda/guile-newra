; -*- mode: scheme; coding: utf-8 -*-

;; (c) Daniel Llorens - 2012-2013, 2015, 2020
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; WIP WIP WIP WIP WIP WIP

;;; Commentary:
;; Concatenation procedures for Newra.
;;; Code:

; BUG Some cases copy and they need not, e.g.
; (define mu (i. 2 3)) (eq? (shared-array-root mu) (shared-array-root (cat 2 mu))) => #f.
; FIXME Should really work with w/rank, etc. As a verb.

(define-module (ploy cat))
(import (srfi :1) (srfi :26) (newra base) (newra lib) (newra lib) (ice-9 match))

(define (

; rank extension would work here except for the need to untranspose if i isn't 0.

(define (plain-cat! i dest . xx)
  (fold (lambda (x base)
          (let ((dc ($. x i)))
            (apply amend! dest x
                   (append (make-list i #t)
                           (list (J dc base))
                           (make-list (- (array-rank x) i 1) #t)))
            (+ base dc)))
        0
        xx)
  dest)

(define (plain-cat! i dest . xx)
  (fold (lambda (x base)
          (let ((dc ($. x i)))
            (apply amend! dest x
                   (append (make-list i #t)
                           (list (J dc base))
                           (make-list (- (array-rank x) i 1) #t)))
            (+ base dc)))
        0
        xx)
  dest)

(define (list-subst l i val)
  (let ((u (list-copy l)))
    (list-set! u i val)
    u))

(define (cat! ir out . xx)
  (let* ((xx (map (lambda (x) (if (array? x) x (make-array x))) xx)))
    (if (> 0 ir)
      (apply cat! 0 out
             (map (lambda (x) (apply make-shared-array
                                x (lambda i (drop i (- ir)))
                                (append (make-list (- ir) 1) ($ x))))
                  xx))
      (let* ((x-largest-rank (fold (lambda (x xm) (if (> (array-rank x) (array-rank xm)) x xm)) (car xx) (cdr xx)))
             (out-rank (max (+ 1 ir) (array-rank x-largest-rank)))
             (dims-to-cat (map (lambda (x)
                                 (if (> (array-rank x) ir)
                                   ($. x ir)
                                   1))
                               xx))
             (out-shape (list-subst (append ($ x-largest-rank)
                                            (make-list (- out-rank (array-rank x-largest-rank)) 1))
                                    ir (apply + dims-to-cat)))
             (o (or out (apply make-typed-array (array-type (car xx)) *unspecified* out-shape)))
             (xx (map (lambda (x dc) (extend-right x (list-subst out-shape ir dc)))
                      xx dims-to-cat)))
        (apply plain-cat! ir o xx)))))

(define (icat! ir out . xx)
  (let* ((xx (map (lambda (x) (if (array? x) x (make-array x))) xx)))
    (if (> 0 ir)
      (apply icat! 0 out
             (map (lambda (x) (apply make-shared-array
                                x (lambda i (drop-right i (- ir)))
                                (append ($ x) (make-list (- ir) 1))))
                  xx))
      (let* ((x-largest-rank (fold (lambda (x xm) (if (> (array-rank x) (array-rank xm)) x xm)) (car xx) (cdr xx)))
             (out-rank (max (+ 1 ir) (array-rank x-largest-rank)))
             (dims-to-cat (map (lambda (x)
                                 (if (> (array-rank x) ir)
                                   ($. x (- (array-rank x) (+ 1 ir)))
                                   1))
                               xx))
             (out-shape (list-subst (append (make-list (- out-rank (array-rank x-largest-rank)) 1)
                                            ($ x-largest-rank))
                                    (- out-rank (+ 1 ir)) (apply + dims-to-cat)))
             (o (or out (apply make-typed-array (array-type (car xx)) *unspecified* out-shape)))
             (xx (map (lambda (x dc) (extend-left x (list-subst out-shape (- out-rank (+ 1 ir)) dc)))
                      xx dims-to-cat)))
        (apply plain-cat! (- out-rank (+ 1 ir)) o xx)))))

(define (cat ir . xx)
  "cat i . xx

   Concatenate arrays xx ... along axis i. The shapes of xx ... must have
   matching prefixes.

   The output array will have the rank of the xx with the largest rank, or (+ 1
   axis), whichever is larger. If necessary, the xx are broadcast to this
   output rank. Where none of the xx provides a dimension, the broadcast
   dimension is 1. The dimensions of the xx must match on all axes, except
   possibly along the axis of concatenation.

   As an extension, if i is negative, the shape of each array xx ... is extended
   by (- i) singleton dimensions on the left and the concatenation is carried
   out along the leftmost axis.

   For example:

   (cat 0 (i. 1 2) (i. 2 2))      => #2((0 1) (0 1) (2 3)))
   (cat 0 #(1 2) #(3 4 5))        => #(1 2 3 4 5))
   (cat -1 #(1 2) #(4 5))         => #2((1 2) (4 5))
   (cat 1 #(1 2) #(4 5))          => #2((1 4) (2 5))
   (cat 0 a #(0 1))               => #(a 0 1)
   (cat 1 a #(0 1))               => #2((a 0) (a 1))
   (cat -1 a #(0 1))              => #2((a a) (0 1))
   (cat 1 #(a b) #2((0 1) (2 3))) => #2((a 0 1) (b 2 3))
   (cat 0 #(a b) #2((0 1) (2 3))) => #2((a a) (b b) (0 1) (2 3))

   See also: (cat!), (icat), (extend-right).
   Cf J append , stitch ,.
   "
  (apply cat! ir #f xx))

(define (icat ir . xx)
  "icat i xx ...

   Concatenate items of rank i of arrays xx ... The shapes of xx ... must have
   matching suffixes.

   The output array will have the rank of the xx with the largest rank, or (+ 1
   i), whichever is larger. If necessary, the xx are broadcast to this output
   rank. Where none of the xx provides a dimension, the broadcast dimension is
   1. The dimensions of the xx must match on all axes, except possibly along the
   axis of concatenation.

   As an extension, if ir is negative, the shape of each array xx ... is
   extended by (- i) singleton dimensions on the right and the
   concatenation is carried out along the rightmost axis.

   (icat ...) always creates a new array and not a shared array. 'icat' stands
   for 'item-cat'.

   For example:

   (icat 0 'a 'b 'c)               => #(a b c)
   (icat 1 'a 'b 'c)               => #2((a) (b) (c))
   (icat 0 #(1 2 3) 4 #(5 6))      => #(1 2 3 4 5 6)
   (icat 0 #2((0 1) (2 3)) #(a b)) => #2((0 1 a b) (2 3 a b)))
   (icat 1 #2((0 1) (2 3)) #(a b)) => #2((0 1) (2 3) (a b))
   (icat 1 #2((0 1)) #(a))         => error, mismatched dimensions along axis 0
   (icat 0 #2((0 1)) #(a))         => #2((0 1 a))
   (icat -1 #(1 2 3) #(a b c))     => #2((1 a) (2 b) (3 c))
   (icat -1 'a #(x y z))            => #2((a x) (a y) (a z))

   See also: (icat!), (cat), (extend-left).

   Longer explanation: suppose the shapes of the arguments are

   (s5 s4 s3 s2 s1 s0)
               (t1 t0)
         (r3 r2 r1 r0)

   The axes are aligned as shown. The numbers indicate the concatenation axis
   for a given value of i. For example, suppose i is 2. Then (s3
   r3), (s1 t1 r1) and (s0 t0 r0) must match. The arguments are broadcast to

   (s5 s4 s3 s2 s1 s0)
   (s5 s4 s3  1 t1 t0)
   (s5 s4 r3 r2 r1 r0)

   and then concatenated along axis (s2 1 r2). The result has shape

   (s5 s4 s3 (+ s2 1 r2) s1 s0).
   "
  (apply icat! ir #f xx))

(export cat cat! icat! icat)
