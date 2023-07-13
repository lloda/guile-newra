; -*- mode: scheme; coding: utf-8 -*-
; Replacement for Guile C-based array system - Tests for ra-format

; (c) Daniel Llorens - 2023
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Run with $GUILE -L mod test/format.scm

(import (newra) (newra print) (srfi srfi-64))

(set! test-log-to-file #f)
(test-begin "format")

(define results
  (ra-map #t (lambda (ra prefix? compact)
               (format #t "\n-------------\nrank ~a compact ~a prefix ~a\n" (ra-rank ra) compact prefix?)
               (ra-format ra #:prefix? prefix? #:compact compact)
               (let ((sq (ra-format ra #f #:prefix? prefix? #:compact compact)))
                 (ra-format sq #:prefix? #f)
                 (ra-dimensions sq)))
          (list->ra 1 (list (ra-i)
                            (ra-i 10)
                            (ra-i 2 5)
                            (ra-i 2 2 5)
                            (ra-i 2 2 2 4)
                            (ra-i 2 2 2 2 4)))
          (ra-transpose (list->ra 1 '(#f #t)) 1)
          (ra-transpose (list->ra 1 '(0 1 2)) 2)))

(ra-format results #:compact 1)

(test-assert "size check"
  (ra-equal? (list->ra 3 '((((1 1) (1 1) (1 1)) ((2 4) (2 4) (2 4)))
                           (((1 21) (1 21) (1 10)) ((2 21) (2 21) (2 10)))
                           (((5 11) (4 11) (2 5)) ((5 11) (4 11) (3 8)))
                           (((5 26) (4 26) (2 18)) ((5 26) (4 26) (3 18)))
                           (((9 25) (7 25) (7 19)) ((9 25) (7 25) (7 19)))
                           (((9 49) (7 49) (7 37)) ((9 49) (7 49) (7 37)))))
             results))


; -----------------------
; the end.
; -----------------------

(define error-count (test-runner-fail-count (test-runner-current)))
(test-end "format")
(exit error-count)
