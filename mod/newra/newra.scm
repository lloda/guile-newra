; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2016-2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Newra is a replacement for Guile's C-based arrays. This is the hub module.
;;; Code:

(define-module (newra newra))

(import (newra base) (newra map) (newra print) (newra read)
        (newra lib) (newra from))

(re-export ra?
           make-ra-raw ra-root ra-zero ra-dims ra-vlen ra-vref ra-vset!
           ra-check %%ra-rank %%ra-root %%ra-zero %%ra-dims
           ra-rank ra-type make-ra-new make-ra-root
           make-aseq aseq? aseq-org aseq-inc aseq-ref
           make-dim dim? dim-len dim-lo dim-hi dim-step dim-ref
           c-dims
           ra-pos ra-offset
           ra-slice ra-cell ra-ref ra-set!

           ra-slice-for-each
           ra-slice-for-each-1 ra-slice-for-each-2 ra-slice-for-each-3 ra-slice-for-each-4
           ra-fill! ra-copy! ra-map! ra-for-each
           ra-equal? ra-any ra-every

           ra-index-map!
           ra-length ra-size make-ra make-typed-ra make-ra-shared ra->list
           ra-dimensions ra-shape ra-offset
           array->ra ra->array as-ra
           ra-i ra-iota
           ra-copy
           ra-reverse ra-transpose ra-order-c? ra-ravel ra-reshape ra-tile
           ra-fold ra-fold*

           ra-from ra-amend!

           list->ra list->typed-ra)
