
; (c) Daniel Llorens - 2019

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; High level array facilities for newra.
;;; Code:

(define-module (newra ploy))

(import (newra newra) (only (srfi srfi-1) fold) (srfi srfi-71) (srfi srfi-26))


; ----------------
; ra-from*
; ----------------

; Similar to from in (ploy ploy), but only beatable cases (rank 0 or type d).
; from & amend! build on this.
