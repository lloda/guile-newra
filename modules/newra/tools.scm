
; Replacement for Guile C-based arrays - Stand on its own
; (c) Daniel Llorens - 2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(define-module (newra tools)
  #:export (time walltime))

(define-syntax time
  (syntax-rules ()
    ((_ e0 ...)
      (let ((start (get-internal-run-time)))
        e0 ...
        (exact->inexact (/ (- (get-internal-run-time) start)
                           internal-time-units-per-second))))))

(define-syntax walltime
  (syntax-rules ()
    ((_ e0 ...)
      (let ((start (get-internal-real-time)))
        e0 ...
        (exact->inexact (/ (- (get-internal-real-time) start)
                           internal-time-units-per-second))))))
