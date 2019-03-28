
; (c) Daniel Llorens - 2018-2019

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Library for Newra - non-essential functions.
;;; Code:

(define-module (newra lib)
  #:export (ra-index-map! ra-i ra-iota as-ra))

(import (newra newra) (only (srfi srfi-1) fold) (srfi srfi-71) (srfi srfi-26))


; ----------------
; ra-index-map!
; ----------------

; Similar to (@ (newra newra) ra-for-each-slice-1) - since we cannot unroll. It
; might be cheaper to go Fortran order (building the index lists back to front);
; should try that. C order and set-cdr! is how oldra does it.
; This function is provided for compatibility with oldra; generally we shouldn't
; be building index lists.

(define (ra-index-map! ra op)
  "ra-index-map! ra op -> ra

   Apply OP to the indices of each element of RA in turn, storing
   the result in the corresponding element.  The value returned and
   the order of application are unspecified.

   This function returns the modified RA.

   For example:

   guile> (define x (make-ra 0 2 3))
   guile> (ra-index-map! x (lambda x x))
   guile> x
   #%2:2:3(((0 0) (0 1) (0 2)) ((1 0) (1 1) (1 2)))

   See also: ra-iota ra-i"

  (let* ((kk (ra-rank ra))
         ((values los lens) ((@@ (newra newra) ra-slice-for-each-check) kk ra)))
    (let loop-rank ((k 0) (ra ra) (ii '()))
      (if (= k kk)
        (ra-set! ra (apply op ii))
        (let* ((lo (vector-ref los k))
               (end (+ lo (vector-ref lens k))))
          (let loop-dim ((i lo))
            (unless (= i end)
              (loop-rank (+ k 1) (ra-slice ra i) (append ii (list i)))
              (loop-dim (+ i 1))))))))
  ra)


; ----------------
; as-ra FIXME partial implementation.
; ----------------

(define* (as-ra ra #:key (type (ra-type ra)) (new? #f))
  (cond ((and (eq? (ra-type ra) type) (not new?)) ra)
        (else (ra-copy! ra (make-ra-new
                            type *unspecified*
                            (apply c-dims (map dim-len (vector->list (ra-dims ra)))))))))


; ----------------
; iota etc.
; ----------------

(define (ra-i . i)
  (make-ra-data (make-dim (fold * 1 i)) (apply c-dims i)))

(define* (ra-iota len #:optional (lo 0) (step 1))
  (make-ra-data (make-dim len lo step) (c-dims len)))
