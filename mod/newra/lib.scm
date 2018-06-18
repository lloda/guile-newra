
; (c) Daniel Llorens - 2018

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Utilities for Newra.
;;; Code:

(define-module (newra lib)
  #:export (ra-i ra-iota as-ra))

(import (newra newra) (only (srfi srfi-1) fold))


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
