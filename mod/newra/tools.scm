; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2017, 2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
; Utilities.
;;; Code:

(define-module (newra tools)
  #:export (define-constant
            time walltime repeat syntax->list throws-exception?))

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

(define-syntax repeat
  (syntax-rules ()
    ((_ n e0 ...) (do ((i 0 (+ i 1))) ((= i n)) e0 ...))))

(define-syntax define-constant
  (syntax-rules ()
    ((_ x e)
     (begin
       (define t e)
       (define-syntax x (identifier-syntax t))))))

; from Chez syntax.stex
(define syntax->list
  (lambda (ls)
    (syntax-case ls ()
      [() '()]
      [(x . r) (cons #'x (syntax->list #'r))])))

(define (throws-exception? k thunk)
  (catch #t
    (lambda () (thunk) #f)
    (lambda args (if (eq? k (car args)) args #f))))
