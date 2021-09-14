; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2016-2019, 2021
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Array iteration.
;;; Code:

(define-module (newra map)
  #:export (ra-slice-for-each ra-slice-for-each-in-order
            ra-slice-for-each-check
            ra-slice-for-each-1 ra-slice-for-each-2 ra-slice-for-each-3 ra-slice-for-each-4
            ra-fill! ra-copy! ra-swap! ra-swap-in-order! ra-map! ra-map-in-order! ra-for-each
            ra-every ra-any ra-equal?))

(import (newra base) (srfi :9) (srfi srfi-9 gnu) (srfi :71) (srfi :2)
        (srfi :26) (srfi srfi-4 gnu) (ice-9 match) (ice-9 control)
        (only (srfi :1) fold every)
        (only (rnrs base) vector-map vector-for-each)
        (only (srfi :43) vector-copy! vector-fill! vector-every)
        (only (rnrs bytevectors) bytevector-copy! bytevector-fill! bytevector?))

; These tables are used to inline specific type combinations in %dispatch.
; We handle fewer types with 2 & 3 arguments to limit the explosion in compile time.

(eval-when (expand load eval)
  (define syntax-accessors-1
    (list (list #'d   #'aseq-ref       #'(cut throw 'no-aseq-set! <...>))
          (list #'#t  #'vector-ref     #'vector-set!                   )
          (list #'f64 #'f64vector-ref  #'f64vector-set!                )
          (list #'u8  #'u8vector-ref   #'u8vector-set!                 )
          ;; (list #'f32 #'f32vector-ref  #'f32vector-set!                )
          ;; (list #'c64 #'c64vector-ref  #'c64vector-set!                )
          ;; (list #'c32 #'c32vector-ref  #'c32vector-set!                )
          ;; (list #'s64 #'s64vector-ref  #'s64vector-set!                )
          ;; (list #'s32 #'s32vector-ref  #'s32vector-set!                )
          ;; (list #'s16 #'s16vector-ref  #'s16vector-set!                )
          ;; (list #'s8  #'s8vector-ref   #'s8vector-set!                 )
          ;; (list #'u64 #'u64vector-ref  #'u64vector-set!                )
          ;; (list #'u32 #'u32vector-ref  #'u32vector-set!                )
          ;; (list #'u16 #'u16vector-ref  #'u16vector-set!                )
          ;; (list #'a   #'string-ref     #'string-set!                   )
          ;; (list #'b   #'bitvector-ref  #'bitvector-set!                )
          ))
  (define syntax-accessors-2
    (list (list #'d   #'aseq-ref       #'(cut throw 'no-aseq-set! <...>))
          (list #'#t  #'vector-ref     #'vector-set!                   )
          (list #'f64 #'f64vector-ref  #'f64vector-set!                )
          (list #'u8  #'u8vector-ref   #'u8vector-set!                 )
          ;; (list #'f32 #'f32vector-ref  #'f32vector-set!                )
          ;; (list #'s64 #'s64vector-ref  #'s64vector-set!                )
          ;; (list #'c64 #'c64vector-ref  #'c64vector-set!                )
          ;; (list #'c32 #'c32vector-ref  #'c32vector-set!                )
          ))
  (define syntax-accessors-3
    (list (list #'#t  #'vector-ref     #'vector-set!                   )
          (list #'f64 #'f64vector-ref  #'f64vector-set!                )
          ;; (list #'s64 #'s64vector-ref  #'s64vector-set!                )
          ;; (list #'c64 #'c64vector-ref  #'c64vector-set!                )
          )))


; ----------------
; ----------------
; ra-slice-for-each, several versions
; ----------------
; ----------------

; ra-slice-for-each-1/2/3/4 do the same thing at increasing levels of inlining
; and complication, except that only ra-slice-for-each-4 supports prefix matching.
; the others are kept for testing.

; Unlike Guile's array-for-each, etc. this one is strict; every dimension must match.
(define (ra-slice-for-each-check k . ra)
  (define (match-len? a b)
    (or (not b) (= a b)))
  (let ((len (make-vector k #f))
        (lo (make-vector k #f)))
    (for-each (lambda (ra)
                (ra-check ra)
                (let ((framek (min k (%%ra-rank ra))))
                  (do ((j 0 (+ j 1))) ((= j framek))
                    (let* ((lenj0 (vector-ref len j))
                           (loj0 (vector-ref lo j))
                           (dimj (vector-ref (%%ra-dims ra) j))
                           (lenj (dim-len dimj))
                           (loj (dim-lo dimj)))
                      (if lenj0
                        (begin
                          (unless (match-len? lenj0 lenj)
                            (throw 'mismatched-lens lenj0 lenj 'at-dim j))
; valid len means los must be matched. lenj0 implies loj0 (cf make-dim) so we can reuse match-len?.
                          (unless (match-len? loj0 loj)
                            (throw 'mismatched-los loj0 loj 'at-dim j)))
                        (begin
                          (vector-set! len j lenj)
                          (vector-set! lo j loj)))))))
      ra)
    (do ((j 0 (+ j 1))) ((= j k))
      (unless (vector-ref len j) (throw 'unset-len-for-dim j len))
      (unless (vector-ref lo j) (throw 'unset-lo-for-dim j lo)))
    (values lo len)))

; slice recursively.
(define (ra-slice-for-each-1 kk op . ra)
  (let ((los lens (apply ra-slice-for-each-check kk ra)))
    (let loop-rank ((k 0) (ra ra))
      (if (= k kk)
        (apply op ra)
        (let* ((lo (vector-ref los k))
               (end (+ lo (vector-ref lens k))))
          (let loop-dim ((i lo))
            (unless (= i end)
              (loop-rank (+ k 1) (map (cut ra-slice <> i) ra))
              (loop-dim (+ i 1)))))))))

(define (make-ra-root-prefix ra framek lo)
  (let ((dims (%%ra-dims ra)))
    (make-ra-root (%%ra-root ra)
                  (if (< framek (%%ra-rank ra)) (vector-drop dims framek) #())
; variant of (ra-offset zero (%%ra-dims ra) framek) to handle lo #f (e.g. on index placeholders).
; FIXME merge or split ra-offset. Shouldn't default to 0 there.
                  (let loop ((k (min framek (%%ra-rank ra))) (pos (%%ra-zero ra)))
                    (if (<= k 0)
                      pos
                      (let* ((k (- k 1))
                             (dim (vector-ref dims k)))
                        (loop k (+ pos (* (vector-ref lo k) (dim-step dim))))))))))

; a single moving slice for each argument.
(define (ra-slice-for-each-2 kk op . frame)
  (let* ((los lens (apply ra-slice-for-each-check kk frame))
; create (rank(ra) - k) slices that we'll use to iterate by bumping their zeros.
         (ra (map (cut make-ra-root-prefix <> kk los) frame)))
    (let loop-rank ((k 0))
      (if (= k kk)
; no fresh slice descriptor like in array-slice-for-each. Should be all right b/c the descriptors can be copied.
        (apply op ra)
        (let  ((lenk (vector-ref lens k)))
          (let loop-dim ((i 0))
            (cond
             ((= i lenk)
              (for-each
                  (lambda (ra frame)
                    (let ((step (dim-step (vector-ref (%%ra-dims frame) k))))
                      (%%ra-zero-set! ra (- (%%ra-zero ra) (* step lenk)))))
                ra frame))
             (else
              (loop-rank (+ k 1))
              (for-each
                  (lambda (ra frame)
                    (let ((step (dim-step (vector-ref (%%ra-dims frame) k))))
                      (%%ra-zero-set! ra (+ (%%ra-zero ra) step))))
                ra frame)
              (loop-dim (+ i 1))))))))))

; moving slice with row-major unrolling.
(define (ra-slice-for-each-3 u op . frame)
  (let ((los lens (apply ra-slice-for-each-check u frame)))
    (let/ec exit
; check early so we can save a step in the loop later.
      (vector-for-each (lambda (len) (when (zero? len) (exit))) lens)
; create (rank(ra) - k) slices that we'll use to iterate by bumping their zeros.
      (let ((ra (map (cut make-ra-root-prefix <> u los) frame)))
; since we'll unroll, special case for rank 0
        (if (zero? u)
          (apply op ra)
; we'll do a normal rank-loop in [0..u) and unroll dimensions [u..k); u must be searched.
          (let* ((u (- u 1))
                 (step (map (lambda (frame) (%%ra-step frame u)) frame))
                 (u len (let loop ((u u) (s step) (len 1))
                          (let ((lenu (vector-ref lens u)))
                            (if (zero? u)
                              (values u (* len lenu))
                              (let ((ss (map (cut * lenu <>) s))
                                    (sm (map (lambda (frame) (%%ra-step frame (- u 1))) frame)))
                                (if (equal? ss sm)
                                  (loop (- u 1) ss (* len lenu))
                                  (values u (* len lenu))))))))
                 (lenm (- len 1)))
            (let loop-rank ((k 0))
              (if (= k u)
; unrolled dimensions.
                (let loop ((i lenm))
; no fresh slice descriptor like in array-slice-for-each. Should be all right b/c the descriptors can be copied.
                  (apply op ra)
                  (cond
                   ((zero? i)
                    (for-each (lambda (ra step)
                                (%%ra-zero-set! ra (- (%%ra-zero ra) (* step lenm))))
                      ra step))
                   (else
                    (for-each (lambda (ra step)
                                (%%ra-zero-set! ra (+ (%%ra-zero ra) step)))
                      ra step)
                    (loop (- i 1)))))
                (let ((lenmk (- (vector-ref lens k) 1)))
                  (let loop-dim ((i lenmk))
                    (loop-rank (+ k 1))
                    (cond
                     ((zero? i)
                      (for-each (lambda (ra frame)
                                  (%%ra-zero-set! ra (- (%%ra-zero ra) (* (%%ra-step frame k) lenmk))))
                        ra frame))
                     (else
                      (for-each (lambda (ra frame)
                                  (%%ra-zero-set! ra (+ (%%ra-zero ra) (%%ra-step frame k))))
                        ra frame)
                      (loop-dim (- i 1))))))))))))))


; ----------------
; ra-slice-for-each, macro version
; ----------------

(define-inlinable (%%ra-step-prefix a k)
  (let ((dims (%%ra-dims a)))
    (if (< k (vector-length dims))
      (dim-step (vector-ref dims k))
      0)))

(define-syntax-rule (%list a ...)
  (list a ...))
(define-syntax-rule (%let ((a ...) (b ...) f) e ...)
  (let ((a (f b)) ...) e ...))
(define-syntax-rule (%stepu n (ra step) ...)
  (begin (%%ra-zero-set! ra (+ (%%ra-zero ra) (* n step))) ...))
(define-syntax-rule (%stepk k n (ra frame) ...)
  (begin (%%ra-zero-set! ra (+ (%%ra-zero ra) (* n (%%ra-step-prefix frame k)))) ...))

(define-syntax-rule (%apply-list a)
  a)
(define-syntax-rule (%apply-let ((a) (b) f) e ...)
  (let ((a (map f b))) e ...))
(define-syntax-rule (%apply-stepu n (ra step))
  (for-each (lambda (ra step) (%stepu n (ra step))) ra step))
(define-syntax-rule (%apply-stepk k n (ra frame))
  (for-each (lambda (ra frame) (%stepk k n (ra frame))) ra frame))

; Extracted from %slice-loop to be specialized for each combination of argument types.
; FIXME only the %op needs to be specialized for types...
(define-syntax %op-loop
  (lambda (stx)
    (syntax-case stx ()
      ((_ %op %stepu %stepk ra_ ...)
       (with-syntax ([(ra ...) (generate-temporaries #'(ra_ ...))]
                     [(frame ...) (generate-temporaries #'(ra_ ...))]
                     [(step ...) (generate-temporaries #'(ra_ ...))])
         #'(lambda (lens lenm u ra ... frame ... step ...)
             (let loop-rank ((k 0))
               (if (= k u)
                 (let loop ((i lenm))
                   (%op ra ...)
                   (cond
                    ((zero? i)
                     (%stepu (- lenm) (ra step) ...))
                    (else
                     (%stepu 1 (ra step) ...)
                     (loop (- i 1)))))
                 (let ((lenmk (- (vector-ref lens k) 1)))
                   (let loop-dim ((i lenmk))
                     (loop-rank (+ k 1))
                     (cond
                      ((zero? i)
                       (%stepk k (- lenmk) (ra frame) ...))
                      (else
                       (%stepk k 1 (ra frame) ...)
                       (loop-dim (- i 1))))))))))))))

(define-syntax %slice-loop
  (lambda (stx)
    (syntax-case stx ()
      ((_ k_ op-once op-loop %list %let frame ...)
       (with-syntax ([(ra ...) (generate-temporaries #'(frame ...))]
                     [(step ...) (generate-temporaries #'(frame ...))]
                     [(s ...) (generate-temporaries #'(frame ...))]
                     [(ss ...) (generate-temporaries #'(frame ...))]
                     [(sm ...) (generate-temporaries #'(frame ...))])
         #`(let* ((k k_)
; create (rank(ra) - k) slices that we'll use to iterate by bumping their zeros.
                  (los lens (apply ra-slice-for-each-check k (%list frame ...))))
             (%let ((ra ...) (frame ...) (cut make-ra-root-prefix <> k los))
; since we'll unroll, special case for rank 0
               (if (zero? k)
; no fresh slice descriptor like in array-slice-for-each. Should be all right b/c the descriptors can be copied.
                 (op-once ra ...)
; check early so we can save a step in the loop later.
                 (when (vector-every positive? lens)
; we'll do a normal rank-loop in [0..u) and unroll dimensions [u..k); u must be searched.
                   (let ((u (- k 1)))
                     (%let ((step ...) (frame ...) (lambda (frome) (%%ra-step-prefix frome u)))
                       (let* ((u len (let loop ((u u) (len 1) (s step) ...)
                                       (let ((lenu (vector-ref lens u)))
                                         (if (zero? u)
                                           (values u (* len lenu))
                                           (%let ((ss ...) (s ...) (cut * lenu <>))
                                             (%let ((sm ...) (frame ...) (lambda (frome) (%%ra-step-prefix frome (- u 1))))
                                               (if (and (equal? ss sm) ...)
                                                 (loop (- u 1) (* len lenu) ss ...)
                                                 (values u (* len lenu)))))))))
                              (lenm (- len 1)))
                         (op-loop lens lenm u ra ... frame ... step ...)))))))))))))

(define (ra-slice-for-each-4 k op . rx)
  (letrec-syntax
      ((%apply-op
        (syntax-rules ()
          ((_ ra) (apply op ra))))
       (%args
        (syntax-rules ()
          ((_ ra ...)
           (%slice-loop k op (%op-loop op %stepu %stepk ra ...)
                        %list %let ra ...)))))
    (apply (case-lambda
            (() (throw 'bad-number-of-arguments))
            ((ra) (%args ra))
            ((ra rb) (%args ra rb))
            ((ra rb rc) (%args ra rb rc))
            (rx (%slice-loop k %apply-op (%op-loop %apply-op %apply-stepu %apply-stepk rx)
                             %apply-list %apply-let rx)))
      rx)))

(define ra-slice-for-each ra-slice-for-each-4)
(define ra-slice-for-each-in-order ra-slice-for-each-4)


; ----------------
; ----------------
; special rank-0 versions, ra-for-each, ra-map!, ra-copy!, ra-equal?
; ----------------
; ----------------

; If op-loop takes 2 args as a rest list, here we must do that as well.
(define-inlinable-case slice-loop-fun
  (case-lambda
   ((op-once op-loop r0)
    (%slice-loop (ra-rank r0)
                 op-once op-loop %list %let r0))
   ((op-once op-loop r0 r1)
    (%slice-loop (max (ra-rank r0) (ra-rank r1))
                 op-once op-loop %list %let r0 r1))
   ((op-once op-loop r0 r1 r2)
    (%slice-loop (max (ra-rank r0) (ra-rank r1) (ra-rank r2))
                 op-once op-loop %list %let r0 r1 r2))
   ((op-once op-loop . r)
    (%slice-loop (fold (lambda (a b) (max b (ra-rank a))) 0 r)
                 op-once op-loop %apply-list %apply-let r))))

; This variant of %op-loop avoids updating/rolling back %%ra-zero and instead keeps indices on the stack. The improvement is somewhat unreasonable...

(define-syntax %sloop
  (lambda (stx)
    (syntax-case stx ()
; can supply special rank-1 op.
      ((_ (%op0 %op1) ra_ ...)
       (with-syntax ([(ra ...) (generate-temporaries #'(ra_ ...))]
                     [(frame ...) (generate-temporaries #'(ra_ ...))]
                     [(step ...) (generate-temporaries #'(ra_ ...))]
                     [(z ...) (generate-temporaries #'(ra_ ...))]
                     [(d ...) (generate-temporaries #'(ra_ ...))])
         #'(slice-loop-fun
            (lambda (ra ...)
              (%op0 (ra (%%ra-root ra) (%%ra-zero ra)) ...))
            (lambda (lens lenm u ra ... frame ... step ...)
              (%let ((d ...) (ra ...) %%ra-root)
                (let loop-rank ((k 0) (z (%%ra-zero ra)) ...)
                  (if (= k u)
                    (%op1 (+ 1 lenm) (ra d z step) ...)
                    (let loop-dim ((i (- (vector-ref lens k) 1)) (z z) ...)
                      (loop-rank (+ k 1) z ...)
                      (unless (zero? i)
                        (loop-dim (- i 1) (+ z (%%ra-step-prefix frame k)) ...)))))))
            ra_ ...)))
; if not, provide default.
      ((_ (%op0) ra_ ...)
       #'(let-syntax
             ((%op1
               (syntax-rules ::: ()
                 ((_ len (ra d z step) :::)
                  (let loop ((i (- len 1)) (z z) :::)
                    (%op0 (ra d z) :::)
                    (unless (zero? i)
                      (loop (- i 1) (+ z step) :::)))))))
           (%sloop (%op0 %op1) ra_ ...))))))

; Use this for %op0 when there's no valid %op0. That may happen when %op1 isn't generic enough (e.g. it only works with step 1) so %sloop is used for %op1 alone.

(define-syntax-rule (%pass ra ...)
  (throw 'bad-usage ra ...))

; FIXME (maybe?) doesn't work for rest list ra (%%ra-zero / %%ra-step are used directly). However, rest list ra cases are slow anyway.

(define-syntax-rule (%apply-sloop %apply-op ra)
  (apply slice-loop-fun
    (lambda ra (%apply-op ra))
    (%op-loop %apply-op %apply-stepu %apply-stepk ra)
    ra))


; -------------------
; dispatch type combinations
; -------------------

(define-syntax %%subop
  (lambda (stx)
    (syntax-case stx ()
      ((_ %op %sloop (vref-ra vset!-ra ra) ...)
       (with-syntax ([(d ...) (generate-temporaries #'(ra ...))]
                     [(z ...) (generate-temporaries #'(ra ...))])
         #'(let-syntax
               ((%op-op
                 (syntax-rules ()
                   ((_ (ra d z) ...)
                    (%op (vref-ra vset!-ra ra d z) ...)))))
             (%sloop (%op-op) ra ...)))))))

; FIXME Zero, one, infinity :-|
; FIXME Partial dispatch? i.e. the first type is supported but not the others.
; FIXME Compile cases on demand.

(define-syntax %dispatch
  (lambda (stx)
    (syntax-case stx ()
      ((_ %sloop %typed-op %op ra)
       #`(case (ra-type ra)
           #,@(map (match-lambda
                     ((tag-ra vref-ra vset!-ra)
                      #`((#,tag-ra)
                         (%%subop %typed-op %sloop
                                  (#,vref-ra #,vset!-ra ra)))))
                syntax-accessors-1)
           (else (%sloop (%op) ra))))
      ((_ %sloop %typed-op %op ra rb)
       #`(case (ra-type ra)
           #,@(map (match-lambda
                     ((tag-ra vref-ra vset!-ra)
                      #`((#,tag-ra)
                         (case (ra-type rb)
                           #,@(map (match-lambda
                                     ((tag-rb vref-rb vset!-rb)
                                      #`((#,tag-rb)
                                         (%%subop %typed-op %sloop
                                                  (#,vref-ra #,vset!-ra ra)
                                                  (#,vref-rb #,vset!-rb rb)))))
                                syntax-accessors-2)
                           (else (%sloop (%op) ra rb)))
                         rb)))
                syntax-accessors-2)
           (else (%sloop (%op) ra rb))))
      ((_ %sloop %typed-op %op ra rb rc)
       #`(case (ra-type ra)
           #,@(map (match-lambda
                     ((tag-ra vref-ra vset!-ra)
                      #`((#,tag-ra)
                         (case (ra-type rb)
                           #,@(map (match-lambda
                                     ((tag-rb vref-rb vset!-rb)
                                      #`((#,tag-rb)
                                         (case (ra-type rc)
                                           #,@(map (match-lambda
                                                     ((tag-rc vref-rc vset!-rc)
                                                      #`((#,tag-rc)
                                                         (%%subop %typed-op %sloop
                                                                  (#,vref-ra #,vset!-ra ra)
                                                                  (#,vref-rb #,vset!-rb rb)
                                                                  (#,vref-rc #,vset!-rc rc)))))
                                                syntax-accessors-3)
                                           (else (%sloop (%op) ra rb rc))))))
                                syntax-accessors-3)
                           (else (%sloop (%op) ra rb rc)))
                         rb)))
                syntax-accessors-3)
           (else (%sloop (%op) ra rb rc)))))))


; -------------------
; zoo
; -------------------

(define (ra-for-each op . rx)
  "
Apply @var{op} to each tuple of elements from arrays @var{rx} ... All the
@var{rx} must have matching shapes.

See also: ra-map! ra-slice-for-each ra-clip
"
  (let-syntax
      ((%typed-fe
        (syntax-rules ()
          ((_ (vref-ra vset!-ra ra da za) ...)
           (op (vref-ra da za) ...))))
       (%fe
        (syntax-rules ()
          ((_ (ra da za) ...)
           (op ((%%ra-vref ra) da za) ...))))
       (%apply-fe
        (syntax-rules ()
          ((_ rx)
           (apply op (map (lambda (ra) ((%%ra-vref ra) (%%ra-root ra) (%%ra-zero ra))) rx))))))
    (apply (case-lambda
            ((ra) (%dispatch %sloop %typed-fe %fe ra))
            ((ra rb) (%dispatch %sloop %typed-fe %fe ra rb))
            ((ra rb rc) (%dispatch %sloop %typed-fe %fe ra rb rc))
            (rx (%apply-sloop %apply-fe rx)))
      rx)))

(define (ra-map! ra op . rx)
  "
Apply @var{op} to each tuple of elements from arrays @var{rx} ... and store the
result in the matching position of array @var{ra}. All the @var{rx} ... must
have matching shapes with @var{ra}.

Returns the updated array @var{ra}

See also: ra-for-each ra-copy! ra-fill! ra-clip
"
  (let-syntax
      ((%typed-map!
        (syntax-rules ()
          ((_ (vref-ra vset!-ra ra da za) (vref-rx vset!-rx rx dx zx) ...)
           (vset!-ra da za
                     (op (vref-rx dx zx) ...)))))
       (%map!
        (syntax-rules ()
          ((_ (ra da za) (rx dx zx) ...)
           ((%%ra-vset! ra) da za
            (op ((%%ra-vref rx) dx zx) ...)))))
       (%apply-map!
        (syntax-rules ()
          ((_ rx)
           ((%%ra-vset! (car rx)) (%%ra-root (car rx)) (%%ra-zero (car rx))
            (apply op (map (lambda (ra) ((%%ra-vref ra) (%%ra-root ra) (%%ra-zero ra))) (cdr rx))))))))
    (apply (case-lambda
            (() (%dispatch %sloop %typed-map! %map! ra))
            ((rb) (%dispatch %sloop %typed-map! %map! ra rb))
            ((rb rc) (%dispatch %sloop %typed-map! %map! ra rb rc))
            (rx (%apply-sloop %apply-map! (cons ra rx))))
      rx)
    ra))

(define ra-map-in-order! ra-map!)

(define (ra-fill! ra fill)
  "
Fill array @var{ra} with value @var{fill}. @var{ra} must be of a type compatible
with @var{fill}.

This function returns the filled array @var{ra}.

See also: ra-copy! ra-map!
"
; These only support step 1. bytevector-fill! can be used after Guile 3.0.8 (check).
; Other options are bli_?setv from BLIS or (contorted) ?axpy from CBLAS.
  (define (line! t)
    (match t
      (#t
       (lambda (fill dst dstart len)
         (vector-fill! dst fill dstart (+ dstart len))))
      ('s
       (lambda (fill dst dstart len)
         (string-fill! dst fill dstart (+ dstart len))))
      ('u8
       (lambda (fill dst dstart len)
         (bytevector-fill! dst fill dstart (+ dstart len))))
      ('d (throw 'cannot-fill-type 'd))
      (t #f)))

; optimization 1
  (cond ((and (positive? (ra-rank ra))
              (= 1 (%%ra-step ra (- (%%ra-rank ra) 1)))
              (and-let* ((line! (line! (%%ra-type ra))))
                (let-syntax
                    ((%fill!
                      (syntax-rules ()
                        ((_ len (ra da za stepa))
; FIXME this assumption depends on traversal order.
                         (if (= 1 stepa)
                           (line! fill da za len)
                           (throw 'bad-assumption-in-ra-fill!))))))
                  (%sloop (%pass %fill!) ra)
                  ra))))
; general case
        (else
         (let-syntax
             ((%typed-fill!
               (syntax-rules ()
                 ((_ (vref-ra vset!-ra ra da za))
                  (vset!-ra da za fill))))
              (%fill!
               (syntax-rules ()
                 ((_ (ra da za))
                  ((%%ra-vset! ra) da za fill)))))
           (%dispatch %sloop %typed-fill! %fill! ra)
           ra))))

(define (ra-copy! ra rb)
  "
Copy the contents of array @var{rb} into array @var{ra}. @var{ra} and @var{rb} must have matching
shapes and be of compatible types.

This function returns the updated array @var{ra}.

See also: ra-fill! ra-map! ra-clip
"
; These only support step 1.
; Other options are bli_?copyv from BLIS or (contorted) ?axpy from CBLAS.
  (define (line! t)
    (match t
      (#t
       (lambda (dst dstart src sstart len)
         (vector-copy! dst dstart src sstart (+ sstart len))))
      ((or 'u8 's8 'u16 's16 'u32 's32 'f32 'c32 'u64 's64 'f64 'c64)
       (let ((bs (bytevector-type-size t)))
         (lambda (dst dstart src sstart len)
           (bytevector-copy! src (* bs sstart) dst (* bs dstart) (* bs len)))))
      ('s
       (lambda (dst dstart src sstart len)
         (string-copy! dst dstart src sstart (+ sstart len))))
      ('d (throw 'cannot-copy-type 'd))
      (t #f)))

  (let ((rankb (ra-rank rb)))
; optimization 1
    (cond ((zero? rankb)
           (ra-fill! ra (ra-ref rb)))
; optimization 2
          ((and (positive? (ra-rank ra))
                (eq? (%%ra-type ra) (%%ra-type rb))
                (= 1 (%%ra-step ra (- rankb 1)) (%%ra-step rb (- rankb 1)))
                (and-let* ((line! (line! (%%ra-type rb))))
; FIXME refactor with the general case below
                  (let-syntax
                       ((%copy!
                        (syntax-rules ()
                          ((_ len (ra da za stepa) (rb db zb stepb))
; FIXME this assumption depends on traversal order.
                           (if (= 1 stepa stepb)
                             (line! da za db zb len)
                             (throw 'bad-assumption-in-ra-copy!))))))
                    (%sloop (%pass %copy!) ra rb)
                    ra))))
; general case
          (else
           (let-syntax
               ((%typed-copy!
                 (syntax-rules ()
                   ((_ (vref-ra vset!-ra ra da za) (vref-rb vset!-rb rb db zb))
                    (vset!-ra da za (vref-rb db zb)))))
                (%copy!
                 (syntax-rules ()
                   ((_ (ra da za) (rb db zb))
                    ((%%ra-vset! ra) da za ((%%ra-vref rb) db zb))))))
             (%dispatch %sloop %typed-copy! %copy! ra rb)
             ra)))))

(define (ra-swap! ra rb)
  "
ra-swap! ra rb

Swap the contents of RB and RA. RA and RB must have matching shapes and be of
compatible types.

See also: ra-copy! ra-fill! ra-map!
"
  (let-syntax
      ((%typed-swap!
        (syntax-rules ()
          ((_ (vref-ra vset!-ra ra da za) (vref-rb vset!-rb rb db zb))
           (let ((c (vref-ra da za)))
             (vset!-ra da za (vref-rb db zb))
             (vset!-rb db zb c)))))
       (%swap!
        (syntax-rules ()
          ((_ (ra da za) (rb db zb))
           (let ((c ((%%ra-vref ra) da za)))
             ((%%ra-vset! ra) da za ((%%ra-vref rb) db zb))
             ((%%ra-vset! rb) db zb c))))))
    (%dispatch %sloop %typed-swap! %swap! ra rb)
    ra))

(define ra-swap-in-order! ra-swap!)

; FIXME refactor ra-any ra-every

(define (ra-every pred? . rx)
  "
ra-every pred? rx ...

RX must be RA of matching shapes. Return true if (PRED? RX ..) is true for every
tuple of matching elements, otherwise return #f.

See also: ra-any ra-equal? ra-fold
"
  (let/ec exit
    (let-syntax
        ((%typed-pred?
          (syntax-rules ()
            ((_ (vref-ra vset!-ra ra da za) ...)
             (unless (pred? (vref-ra da za) ...)
               (exit #f)))))
         (%pred?
          (syntax-rules ()
            ((_ (ra da za) ...)
             (unless (pred? ((%%ra-vref ra) da za) ...)
               (exit #f))))))
      (or (null? rx)
          (begin
            (apply (case-lambda
                    ((ra) (%dispatch %sloop %typed-pred? %pred? ra))
                    ((ra rb) (%dispatch %sloop %typed-pred? %pred? ra rb))
                    ((ra rb rc) (%dispatch %sloop %typed-pred? %pred? ra rb rc))
                    (rx (apply ra-for-each (lambda x (unless (apply pred? x) (exit #f))) rx)))
              rx)
            #t)))))

(define (ra-any pred? . rx)
  "
ra-any pred? rx ...

RX must be ra of matching shapes. Return (PRED? RXi ..) is that is true for some
tuple RXi ... of matching elements, otherwise return #f.

For example:

; find i, j such that A(i, j) is true

(define I (ra-iota #f))
(define J (ra-transpose (ra-iota #f) 1))
(ra-any (lambda (a i j) (and a (vector i j))) A I J)

See also: ra-every ra-equal? ra-fold
"
  (let/ec exit
    (let-syntax
        ((%typed-pred?
          (syntax-rules ()
            ((_ (vref-ra vset!-ra ra da za) ...)
             (and=> (pred? (vref-ra da za) ...) exit))))
         (%pred?
          (syntax-rules ()
            ((_ (ra da za) ...)
             (and=> (pred? ((%%ra-vref ra) da za) ...) exit)))))
      (or (null? rx)
          (begin
            (apply (case-lambda
                    ((ra) (%dispatch %sloop %typed-pred? %pred? ra))
                    ((ra rb) (%dispatch %sloop %typed-pred? %pred? ra rb))
                    ((ra rb rc) (%dispatch %sloop %typed-pred? %pred? ra rb rc))
                    (rx (apply ra-for-each (lambda x (and=> (apply pred? x) exit)) rx)))
              rx)
            #f)))))

(define (equal-shapes? ra . rx)
  (let/ec exit
    (let ((da (ra-dims ra))
          (ta (ra-type ra)))
      (for-each (lambda (rb)
                  (let ((db (ra-dims rb)))
                    (unless (and (eq? ta (ra-type rb))
                                 (= (vector-length da) (vector-length db)))
                      (exit #f))
                    (vector-for-each
                     (lambda (da db)
                       (unless (and (eqv? (dim-lo da) (dim-lo db)) (eqv? (dim-len da) (dim-len db)))
                         (exit #f)))
                     da db)))
        rx)
      #t)))

; FIXME it's a bit silly that (ra-transpose (ra-i 9) 1) isn't ra-equal? to
; itself because dead axes don't match each other.
; FIXME built-in array-equal? says 'equal? or array-equal?' for the elements.

(define (ra-equal? . rx)
  "
ra-equal? rx ...

Return #t if the arrays @var{rx} ... have the same shapes and types and their
corresponding elements are @code{equal?}, or #f otherwise.

See also: ra-map! ra-for-each
"
  (let/ec exit
    (let-syntax
        ((%typed-equal?
          (syntax-rules ()
            ((_ (vref-ra vset!-ra ra da za) ...)
             (unless (equal? (vref-ra da za) ...)
               (exit #f)))))
         (%equal?
          (syntax-rules ()
            ((_ (ra da za) ...)
             (unless (equal? ((%%ra-vref ra) da za) ...)
               (exit #f))))))
      (and (apply equal-shapes? rx)
; shortcut?
           (or (let* ((ra (car rx))
                      (za (ra-zero ra))
                      (ra (ra-root ra)))
                 (let loop ((rx (cdr rx)))
                   (or (null? rx)
                       (and (= za (ra-zero (car rx)))
                            (eq? ra (ra-root (car rx)))
                            (loop (cdr rx))))))
; no, have to go element per element.
               (begin
                 (apply (case-lambda
                         ((ra rb) (%dispatch %sloop %typed-equal? %equal? ra rb))
                         (rx (apply ra-for-each (lambda x (unless (apply equal? x) (exit #f))) rx)))
                   rx)
                 #t))))))
