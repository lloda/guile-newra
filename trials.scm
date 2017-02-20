
; Replacement for Guile C-based arrays - WIP
; (c) Daniel Llorens - 2016-2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(import (rnrs io ports) (srfi srfi-1) (srfi srfi-4 gnu) (rnrs base) (srfi srfi-26)
        (newra newra) (newra print))

(define array-print-prefix (@@ (ice-9 arrays) array-print-prefix))
(define array-print (@@ (ice-9 arrays) array-print))

; this is a direct translation of scm_i_print_array_dimension() in arrays.c.
(define (array-print* a port)
  (define lo caar)
  (define hi cadar)
  (array-print-prefix a port)
  (let ((s (array-shape a))
        (i (shared-array-increments a))
        (r (shared-array-root a))
        (b (shared-array-offset a)))
    (let ((ref (case (array-type r)
                 ((#t) vector-ref)
                 ((c64) c64vector-ref)
                 ((c32) c32vector-ref)
                 ((f64) f64vector-ref)
                 ((f32) f32vector-ref)
                 ((s64) s64vector-ref)
                 ((s32) s32vector-ref)
                 ((s16) s16vector-ref)
                 ((s8)  s8vector-ref)
                 ((u64) u64vector-ref)
                 ((u32) u32vector-ref)
                 ((u16) u16vector-ref)
                 ((u8) u8vector-ref)
                 ((a) string-ref)
                 ((b) bitvector-ref)
                 (else (throw 'bad-type (array-type r))))))
; special case
      (if (zero? (array-rank a))
        (begin
          (display #\( port)
          (display (ref b) port)
          (display #\) port))
        (let loop ((ss s) (ii i) (b b))
          (if (null? ss)
            (display (ref r b) port)
            (let ((lo (lo ss))
                  (hi (hi ss))
                  (i (car ii)))
              (put-char port #\()
              (do ((j lo (+ 1 j))
                   (b b (+ b i)))
                  ((> j hi))
                (loop (cdr ss) (cdr ii) b)
                (when (< j hi)
                  (put-char port #\space)))
              (put-char port #\)))))))))

(define a (make-array 9 10 10 10 10 10))
(define ra (array->ra a))
,time (call-with-output-file "/dev/null" (cut display a <>))
,time (call-with-output-file "/dev/null" (cut array-print a <>))
,time (call-with-output-file "/dev/null" (cut array-print* a <>))
,time (call-with-output-file "/dev/null" (cut display ra <>))

(define a (make-array 9 50 50 50))
(define ra (array->ra a))
,time (call-with-output-file "/dev/null" (cut display a <>))
,time (call-with-output-file "/dev/null" (cut array-print a <>))
,time (call-with-output-file "/dev/null" (cut array-print* a <>))
,time (call-with-output-file "/dev/null" (cut display ra <>))

(define a (make-array 9 400 400))
(define ra (array->ra a))
,time (call-with-output-file "/dev/null" (cut display a <>))
,time (call-with-output-file "/dev/null" (cut array-print a <>))
,time (call-with-output-file "/dev/null" (cut array-print* a <>))
,time (call-with-output-file "/dev/null" (cut display ra <>))
