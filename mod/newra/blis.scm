; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2023
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; A place to hang have-blis? without any other dependency.
;;; Code:

(define-module (newra blis)
  #:export (have-blis?))

(eval-when (expand load eval)
  (define have-blis? (catch #t (lambda () (import (ffi blis)) #t) (const #f))))
