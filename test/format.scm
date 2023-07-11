; -*- mode: scheme; coding: utf-8 -*-
; Replacement for Guile C-based array system - Tests for ra-format

; (c) Daniel Llorens - 2023
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

; Run with $GUILE -L mod test/format.scm
; FIXME actually check something

(import (newra) (newra print) (srfi srfi-64))

(set! test-log-to-file #f)
(test-begin "format")

(for-each
    (lambda (ra)
      (for-each
          (lambda (prefix?)
            (for-each
                (lambda (compact)
                  (format #t "\n-------------\nrank ~a compact ~a prefix ~a\n" (ra-rank ra) compact prefix?)
                  (ra-format ra #:prefix? prefix? #:compact compact)
                  (ra-format (ra-format ra #f #:prefix? prefix? #:compact compact) #:prefix? #f))
              '(0 1 2)))
        '(#f #t)))
  (list (ra-i)
        (ra-i 10)
        (ra-i 2 5)
        (ra-i 2 2 5)
        (ra-i 2 2 2 4)
        (ra-i 2 2 2 2 4)))


; -----------------------
; the end.
; -----------------------

(define error-count (test-runner-fail-count (test-runner-current)))
(test-end "format")
(exit error-count)
