
; Replacement for Guile C-based array system - WIP
; (c) Daniel Llorens - 2016-2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(import (newra newra) (newra print) (newra tools)
        (only (rnrs base) vector-map) (only (srfi srfi-1) fold)
        (srfi srfi-26) (ice-9 match))

(define ra0 (array->ra #(1 2 3)))
(define ra1 (array->ra #@1(1 2 3)))
(define ra2 (array->ra #2((1 2) (3 4))))
(define ra3 (array->ra #2@1@1((1 2) (3 4))))

(make-ra-c #t 0 2 3)
(make-ra-c #t 0 3 2)
(make-ra-c #t 0 '(1 3) 2)
(define ra4 (make-ra-c #t 0 '(1 3) '(1 2)))
(array-index-map! (ra-data ra4) (lambda i i))

; now array-cell, etc.

(define ra0 (make-ra-c (make-dim 10) 10))
(define ra2 (make-ra (make-dim 10) 0 (vector (make-dim 10))))
