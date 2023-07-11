; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2017-2023
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Printer for ra objects. They start with #% instead of #, otherwise the syntax
;; is the same as for regular Guile arrays. Loading this module installs the
;; printer. This module also provides a pretty-printer (ra-format).
;;; Code:

(define-module (newra print)
  #:export (ra-print-prefix ra-print ra-format
            *ra-print* *ra-parenthesized-rank-zero*))

(import (rnrs io ports) (rnrs base) (srfi srfi-1) (srfi srfi-4 gnu) (srfi srfi-26) (srfi srfi-71)
        (ice-9 match) (ice-9 control)
        (newra base) (newra map) (newra cat) (newra from) (newra lib) (newra reshape))

(define *ra-print*
  (make-parameter #f
    (lambda (x) (match x
                  ((or 'box 'box1 'box2 'default #f (? procedure?)) x)
                  (x (throw 'bad-argument-to-*ra-print* x))))))

(define *ra-parenthesized-rank-zero*
  (make-parameter #t))

; FIXME still need to extend (truncated-print).

(define* (ra-print-prefix ra port #:key (dims? #t))
  (display #\# port)
  (display #\% port)
  (display (ra-rank ra) port)
  (let ((type (ra-type ra)))
    (unless (eq? #t type)
      (display type port)))
  (vector-for-each
   (lambda (dim)
     (let ((lo (dim-lo dim)))
       (unless (or (not lo) (zero? lo))
         (display #\@ port)
         (display (or lo 'f) port)))
     (when dims?
       (display #\: port)
       (display (match (dim-len dim)
; print len of dead axes with 'd and of infinite axes with 'f.
                  (#f (if (zero? (dim-step dim)) 'd  'f))
                  (len len))
                port)))
   (ra-dims ra)))

(define* (ra-print ra port #:key (dims? #t))
  (ra-print-prefix ra port #:dims? dims?)
  (let ((base (ra-offset (ra-zero ra) (ra-dims ra)))
        (ref (cute (ra-vref ra) (ra-root ra) <>))
        (rank (ra-rank ra)))
; special case
    (if (zero? rank)
      (if (*ra-parenthesized-rank-zero*)
        (begin
          (display #\( port)
          (write (ref base) port)
          (display #\) port))
        (begin
          (display #\space port)
          (write (ref base) port)))
      (let loop ((k 0) (b base))
        (let* ((dim (vector-ref (ra-dims ra) k))
               (i (dim-step dim))
               (lo (dim-lo dim))
; print dead axes as if of size 1. Infinite arrays aren't printed (FIXME?)
               (len (or (dim-len dim) (if (zero? i) 1 #f))))
          (when len
            (let ((hi (+ (or lo 0) len -1)))
              (display #\( port)
              (cond
               ((= (- rank 1) k)
                (do ((j (or lo 0) (+ 1 j)) (b b (+ b i)))
                    ((> j hi))
                  (write (ref b) port)
                  (when (< j hi)
                    (display #\space port))))
               (else
                (do ((j (or lo 0) (+ 1 j)) (b b (+ b i)))
                    ((> j hi))
                  (loop (+ k 1) b)
                  (when (< j hi)
                    (display #\space port)))))
              (display #\) port))))))))

(define* (sc-print sc #:optional (o #t))
  (let ((o (match o
             (#t (current-output-port))
             (#f (throw 'bad-output-spec))
             (o o))))
    (ra-slice-for-each 1
      (lambda (line)
        (ra-for-each (cut display <> o) line)
        (newline o))
      sc)))

(define arts (make-ra-root (vector "┆╌┌┐└┘     " "│─┌┐└┘├┤┬┴┼" "║═╔╗╚╝╠╣╦╩╬" "┃━┏┓┗┛┣┫┳┻╋"
                                   "░░░░░░░░░░░" "▒▒▒▒▒▒▒▒▒▒▒" "▓▓▓▓▓▓▓▓▓▓▓" "████████████")))

(define (vector-any pred? v)
  (let/ec exit
    (vector-for-each (lambda (e) (and=> (pred? e) exit)) v)
    #f))

; FIXME if a cell prints as nothing (e.g. "" with compact >0) then it shouldn't take up vertical space.
; FIXME compact >0 rank <=2 should avoid borders at all.

(define* (ra-format ra #:optional (port #t) #:key (fmt "~a") (prefix? #t) (compact 0))
  (define prefix (and prefix? (call-with-output-string (cut ra-print-prefix ra <>))))
  (let ((ra (if (vector-any (lambda (d)
                              (and (not (dim-len d))
                                   (not (zero? (dim-step d)))))
                            (ra-dims ra))
; for arrays with infinite axes, print just the prefix.
              (make-ra #f 0)
; for arrays with dead axes, print them as if the len was 1, but preserve the prefix.
              (ra-singletonize ra))))
    (define tostring (if (string? fmt) (cut format #f fmt <>) fmt))
; size the cells
    (define s (ra-map! (apply make-ra #f (ra-dimensions ra))
                       (lambda (x)
                         (if (ra? x)
                           (ra-format x #f #:fmt fmt #:prefix? prefix? #:compact compact)
                           (ra-tile (make-ra-root (tostring x)) 0 1)))
                       ra))
; vertical axes go in dimv, horizontal axes in dimh
    (define-values (dimv dimh)
      (let* ((q r (euclidean/ (ra-rank s) 2))
             (a (ra-iota (+ q r) 0 2))
             (b (ra-iota q 1 2)))
        (if (zero? r)
          (values a b)
          (values b a))))
    (define extrav (> (ra-len dimv) (if (< compact 2) 0 1)))
    (define extrah (> (ra-len dimh) (if (< compact 2) 0 1)))
    (define (lengths dimv dimh k compact)
      (let* ((sq (apply ra-untranspose s (ra->list (ra-cat #f 0 dimh dimv))))
             (l (apply make-ra 0 (drop (ra-dimensions sq) (ra-len dimh))))
             (inner-compact? (if (zero? compact)
                               (= (ra-len dimv) 0)
                               (>= (ra-len dimv) 0)))
             (border (if inner-compact? 0 1)))
        (ra-slice-for-each-in-order (ra-len dimh)
          (lambda (w) (ra-map! l (lambda (l w) (max l (+ border (ra-len w k)))) l w))
          sq)
; FIXME handle border entirely here
        (when (and inner-compact? (match k (0 extrav) (1 extrah)))
          (let ((ll (ra-from l (dots) ((match k (0 dim-hi) (1 dim-lo)) (vector-ref (ra-dims l) (- (ra-rank l) 1))))))
            (ra-map! ll (cut + <> 1) ll)))
        l))
    (define lv (lengths dimv dimh 0 (match compact (0 0) (1 1) (2 2))))
    (define lh (lengths dimh dimv 1 (match compact (0 0) (1 0) (2 2))))
    (define tv (ra-fold + (if extrav 0 -1) lv))
    (define th (ra-fold + (if extrah 0 -1) lh))
; compute positions for grid and cells
    (define (scan! a) (let ((s 0)) (ra-map-in-order! a (lambda (c) (let ((d s)) (set! s (+ s c)) d)) a)))
    (define (scan-0 a) (scan! (ra-copy a)))
    (define (scan-1 a) (scan! (ra-cat #f 0 a (make-ra 0))))
    (define (marks l k)
      (and (>= k 0)
           (let ((m (apply make-ra 0 (take (ra-dimensions l) (+ k 1)))))
             (ra-slice-for-each (+ k 1) (lambda (l m) (set! (m) (ra-fold + 0 l)) m) l m)
             (scan-1 (ra-ravel m)))))
; make screen, adding line for prefix if necessary
    (define prefix-lines (if (and prefix (not extrav)) 1 0))
    (define scc (make-typed-ra 'a #\space
                               (+ 1 tv prefix-lines)
                               (max (if prefix (string-length prefix) 0) (+ 1 th))))
    (define sc (ra-from scc (ra-iota (- (ra-len scc) prefix-lines) prefix-lines)))
    (define (char k n) (string-ref (ra-ref arts (+ (if (positive? compact) 0 1) k)) n))
    (define (line-0 sc k range at) (ra-amend! sc (char k 0) range at))
    (define (line-1 sc k range at) (ra-amend! sc (char k 1) at range))
    (cond
     ((zero? (ra-rank ra))
      (let ((s (s))) (ra-copy! (ra-clip sc s) s))) ; align left
     ((zero? (ra-size ra)) #f)
     (else
; print grid
      (let loop ((k (max 0 (- compact 1))))
        (let* ((m0 (marks lv (- (ra-len dimv) 1 k)))
               (m1 (marks lh (- (ra-len dimh) 1 k)))
               (>m0< (and m0 (ra-from m0 (ra-iota (- (ra-len m0) 2) 1))))
               (>m1< (and m1 (ra-from m1 (ra-iota (- (ra-len m1) 2) 1)))))
          (cond ((and m0 m1)
; horiz + vert
                 (if (and (positive? compact) (zero? k))
                   (begin
                     (line-1 sc k (ra-iota (+ 1 th) 0) (ra-ref m0 0))
                     (line-1 sc k (ra-iota (+ 1 th) 0) (ra-ref m0 (- (ra-len m0) 1)))
                     (line-0 sc k (ra-iota (+ 1 tv) 0) (ra-ref m1 0))
                     (line-0 sc k (ra-iota (+ 1 tv) 0) (ra-ref m1 (- (ra-len m1) 1))))
                   (begin
                     (ra-for-each (lambda (m0) (line-1 sc k (ra-iota (+ 1 th) 0) m0)) m0)
                     (ra-for-each (lambda (m1) (line-0 sc k (ra-iota (+ 1 tv) 0) m1)) m1)))
; inner crosses
                 (if (positive? compact)
                   (when (> k 0)
                     (ra-for-each (lambda (m0 m1) (ra-set! sc (char k 10) m0 m1))
                                  >m0< (ra-transpose >m1< 1)))
                   (ra-for-each (lambda (m0 m1) (ra-set! sc (char k 10) m0 m1))
                                >m0< (ra-transpose >m1< 1)))
; edge crosses
                 (unless (and (positive? compact) (zero? k))
                   (ra-for-each (lambda (m0)
                                  (ra-set! sc (char k 6) m0 0)
                                  (ra-set! sc (char k 7) m0 th))
                                >m0<)
                   (ra-for-each (lambda (m1)
                                  (ra-set! sc (char k 8) 0 m1)
                                  (ra-set! sc (char k 9) tv m1))
                                >m1<))
; corners
                 (ra-set! sc (char k 2) 0 0)
                 (ra-set! sc (char k 3) 0 th)
                 (ra-set! sc (char k 4) tv 0)
                 (ra-set! sc (char k 5) tv th)
                 (loop (+ k 1)))
                (m1
                 (if (and (positive? compact) (zero? k))
                   (begin
                     (line-0 sc k (ra-iota (+ tv 1) 0) 0)
                     (line-0 sc k (ra-iota (+ tv 1) 0) (ra-ref m1 (- (ra-len m1) 1))))
                   (ra-for-each (lambda (m1) (line-0 sc k (ra-iota (+ tv 1) 0) m1)) m1)))
                (else #f))))
; print cells
      (ra-for-each
       (lambda (sq ov lv oh lh)
         (ra-copy! (ra-from sc
                            (ra-iota (ra-len sq 0) (+ ov (if extrav 1 0)))
                            (ra-iota (ra-len sq 1) (+ oh lh (- (ra-len sq 1))))) ; align right
                   sq))
       (apply ra-untranspose s (ra->list (ra-cat #f 0 dimv dimh)))
       (apply ra-reshape (scan-0 (ra-ravel lv)) 0 (ra-dimensions lv))
       lv
       (ra-transpose (apply ra-reshape (scan-0 (ra-ravel lh)) 0 (ra-dimensions lh)) (ra-rank lv))
       (ra-transpose lh (ra-rank lv)))))
; print prefix
    (when prefix
      (ra-amend! scc (make-ra-root prefix) 0 (ra-iota (string-length prefix))))
    (if port
      (sc-print scc port)
      scc)))

(struct-set! (@ (newra base) <ra-vtable>) vtable-index-printer
             (lambda (ra o)
               (match (*ra-print*)
                 ('box (newline o) (ra-format ra o))
                 ('box1 (newline o) (ra-format ra o #:compact 1))
                 ('box2 (newline o) (ra-format ra o #:compact 2))
                 ((or 'default #f) (ra-print ra o))
                 (f (f ra o)))))
