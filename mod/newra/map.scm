; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2016-2019
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Array iteration.
;;; Code:

(define-module (newra map)
  #:export (ra-slice-for-each
            ra-slice-for-each-check
            ra-slice-for-each-1 ra-slice-for-each-2 ra-slice-for-each-3 ra-slice-for-each-4
            ra-fill! ra-copy! ra-equal? ra-map! ra-for-each))

(import (newra base) (srfi :9) (srfi srfi-9 gnu) (only (srfi :1) fold every) (srfi :8)
        (srfi srfi-4 gnu) (srfi :26) (ice-9 match) (ice-9 control)
        (only (rnrs base) vector-map vector-for-each))

; These tables are used to inline specific type cases in %dispatch.
; We handle fewer types with 2 & 3 arguments to limit the combinatory explosion.

(eval-when (expand load eval)
  (define syntax-accessors-1
    (list (list #'#t  #'vector-ref     #'vector-set!                   )
          (list #'c64 #'c64vector-ref  #'c64vector-set!                )
          (list #'c32 #'c32vector-ref  #'c32vector-set!                )
          (list #'f64 #'f64vector-ref  #'f64vector-set!                )
          (list #'f32 #'f32vector-ref  #'f32vector-set!                )
          (list #'s64 #'s64vector-ref  #'s64vector-set!                )
          (list #'s32 #'s32vector-ref  #'s32vector-set!                )
          (list #'s16 #'s16vector-ref  #'s16vector-set!                )
          (list #'s8  #'s8vector-ref   #'s8vector-set!                 )
          (list #'u64 #'u64vector-ref  #'u64vector-set!                )
          (list #'u32 #'u32vector-ref  #'u32vector-set!                )
          (list #'u16 #'u16vector-ref  #'u16vector-set!                )
          (list #'u8  #'u8vector-ref   #'u8vector-set!                 )
          (list #'a   #'string-ref     #'string-set!                   )
          (list #'b   #'bitvector-ref  #'bitvector-set!                )
          (list #'d   #'dim-ref        #'(cut throw 'no-dim-set! <...>))
          ))
  (define syntax-accessors-2
    (list (list #'#t  #'vector-ref     #'vector-set!                   )
          (list #'c64 #'c64vector-ref  #'c64vector-set!                )
          (list #'c32 #'c32vector-ref  #'c32vector-set!                )
          (list #'f64 #'f64vector-ref  #'f64vector-set!                )
          (list #'f32 #'f32vector-ref  #'f32vector-set!                )
          (list #'s64 #'s64vector-ref  #'s64vector-set!                )
          ))
  (define syntax-accessors-3
    (list (list #'#t  #'vector-ref     #'vector-set!                   )
          (list #'c64 #'c64vector-ref  #'c64vector-set!                )
          (list #'f64 #'f64vector-ref  #'f64vector-set!                )
          (list #'s64 #'s64vector-ref  #'s64vector-set!                )
          )))


; ----------------
; ra-slice-for-each, several versions
; ----------------

; ra-slice-for-each-1/2/3/4 do the same thing at increasing levels of inlining
; and complication. We keep them to test them against each other. The last one
; is the default.

; Unlike Guile's array-for-each, etc. this one is strict —every dimension must match.
(define (ra-slice-for-each-check k . ra)
  (unless (pair? ra)
    (throw 'missing-arguments))
  (for-each check-ra ra)
  (unless (<= k (%%ra-rank (car ra)))
    (throw 'bad-frame-rank k (%%ra-rank (car ra))))
  (unless (every (lambda (rai) (= (%%ra-rank (car ra)) (%%ra-rank rai))) (cdr ra))
    (throw 'bad-ranks k (map ra-rank ra)))
  (let* ((dims (map (compose (cut vector-take <> k) %%ra-dims) ra))
         (lo (vector-map dim-lo (car dims)))
         (len (vector-map dim-len (car dims))))
    (unless (every (lambda (rb) (equal? lo (vector-map dim-lo rb))) (cdr dims))
      (throw 'mismatched-lo lo))
    (unless (every (lambda (rb) (equal? len (vector-map dim-len rb))) (cdr dims))
      (throw 'mismatched-len len))
    (values lo len)))

; naive, slice recursively.
(define (ra-slice-for-each-1 kk op . ra)
  (receive (los lens) (apply ra-slice-for-each-check kk ra)
; we pick a (-k)-slice for each ra and then just move along.
    (let loop-rank ((k 0) (ra ra))
      (if (= k kk)
        (apply op ra)
        (let* ((lo (vector-ref los k))
               (end (+ lo (vector-ref lens k))))
          (let loop-dim ((i lo))
            (unless (= i end)
              (let ((rai (map (cut ra-slice <> i) ra)))
                (loop-rank (+ k 1) rai)
                (loop-dim (+ i 1))))))))))

; moving slice
(define (ra-slice-for-each-2 kk op . ra)
  (receive (los lens) (apply ra-slice-for-each-check kk ra)
; create (rank(ra) - k) slices that we'll use to iterate by bumping their zeros.
    (let ((frame ra)
          (ra (map (lambda (ra)
                     (make-ra-raw (%%ra-data ra)
                                  (ra-pos-first (%%ra-zero ra) (vector-take (%%ra-dims ra) kk))
                                  (vector-drop (%%ra-dims ra) kk)))
                ra)))
      (let loop-rank ((k 0))
        (if (= k kk)
; no fresh slice descriptor like in array-slice-for-each. See below.
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
                (loop-dim (+ i 1)))))))))))

; moving slice, row-major unrolling.
(define (ra-slice-for-each-3 u op . ra)
  (receive (los lens) (apply ra-slice-for-each-check u ra)
    (let/ec exit
; check early so we can save a step in the loop later.
      (vector-for-each (lambda (len) (when (zero? len) (exit))) lens)
; create (rank(ra) - k) slices that we'll use to iterate by bumping their zeros.
      (let* ((frame ra)
             (ra (map (lambda (ra)
                        (make-ra-raw (%%ra-data ra)
                                     (ra-pos-first (%%ra-zero ra) (vector-take (%%ra-dims ra) u))
                                     (vector-drop (%%ra-dims ra) u)))
                   ra)))
; since we'll unroll, special case for rank 0
        (if (zero? u)
          (apply op ra)
; we'll do a normal rank-loop in [0..u) and unroll dimensions [u..k); u must be found.
; the last axis of the frame can always be unrolled, so we start checking from the one before.
          (let* ((u (- u 1))
                 (step (map (lambda (frame) (%%ra-step frame u)) frame)))
            (receive (u len)
                (let loop ((u u) (s step) (len 1))
                  (let ((lenu (vector-ref lens u)))
                    (if (zero? u)
                      (values u (* len lenu))
                      (let ((ss (map (cut * lenu <>) s))
                            (sm (map (lambda (frame) (%%ra-step frame (- u 1))) frame)))
                        (if (equal? ss sm)
                          (loop (- u 1) ss (* len lenu))
                          (values u (* len lenu)))))))
              (let ((lenm (- len 1)))
                (let loop-rank ((k 0))
                  (if (= k u)
; unrolled dimensions.
                    (let loop ((i lenm))
; no fresh slice descriptor like in array-slice-for-each. That should be all right in newra, b/c the descriptors can be copied.
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
                          (loop-dim (- i 1))))))))))))))))


; ----------------
; ra-slice-for-each, macro version
; ----------------

(define-syntax %list
  (syntax-rules ()
    ((_ a ...) (list a ...))))
(define-syntax %let
  (syntax-rules ()
    ((_ ((a ...) (b ...) f) e ...)
     (let ((a (f b)) ...) e ...))))
(define-syntax %stepu
  (syntax-rules ()
    ((_ n (ra step) ...)
     (begin (%%ra-zero-set! ra (+ (%%ra-zero ra) (* n step))) ...))))
(define-syntax %stepk
  (syntax-rules ()
    ((_ k n (ra frame) ...)
     (begin (%%ra-zero-set! ra (+ (%%ra-zero ra) (* n (%%ra-step frame k)))) ...))))

(define-syntax %apply-list
  (syntax-rules ()
    ((_ a) a)))
(define-syntax %apply-let
  (syntax-rules ()
    ((_ ((a) (b) f) e ...)
     (let ((a (map f b))) e ...))))
(define-syntax %apply-stepu
  (syntax-rules ()
    ((_ n (ra step))
     (for-each (lambda (ra step) (%stepu n (ra step))) ra step))))
(define-syntax %apply-stepk
  (syntax-rules ()
    ((_ k n (ra frame))
     (for-each (lambda (ra frame) (%stepk k n (ra frame))) ra frame))))

; Extracted from %slice-loop to be specialized for each combination of argument types.
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
                 (let loop-unrolled ((i lenm))
                   (%op ra ...)
                   (cond
                    ((zero? i)
                     (%stepu (- lenm) (ra step) ...))
                    (else
                     (%stepu 1 (ra step) ...)
                     (loop-unrolled (- i 1)))))
                 (let ((lenmk (- (vector-ref lens k) 1)))
                   (let loop-dim ((i lenmk))
                     (loop-rank (+ k 1))
                     (cond
                      ((zero? i)
                       (%stepk k (- lenmk) (ra frame) ...))
                      (else
                       (%stepk k 1 (ra frame) ...)
                       (loop-dim (- i 1))))))))))))))

; This variant of %op-loop avoids updating/rolling back %%ra-zero and instead
; keeps indices on the stack. The improvement is somewhat unreasonable...
; FIXME (maybe?) unusable for list ra (since %%ra-zero/%%ra-step are used directly).
(define-syntax %op-loop-z
  (lambda (stx)
    (syntax-case stx ()
      ((_ %op ra_ ...)
       (with-syntax ([(ra ...) (generate-temporaries #'(ra_ ...))]
                     [(frame ...) (generate-temporaries #'(ra_ ...))]
                     [(step ...) (generate-temporaries #'(ra_ ...))]
                     [(z ...) (generate-temporaries #'(ra_ ...))])
         #'(lambda (lens lenm u ra ... frame ... step ...)
             (let loop-rank ((k 0) (z (%%ra-zero ra)) ...)
               (if (= k u)
                 (let loop-unrolled ((i lenm) (z z) ...)
                   (%op (ra z) ...)
                   (unless (zero? i)
                     (loop-unrolled (- i 1) (+ z step) ...)))
                 (let loop-dim ((i (- (vector-ref lens k) 1)) (z z) ...)
                   (loop-rank (+ k 1) z ...)
                   (unless (zero? i)
                     (loop-dim (- i 1) (+ z (%%ra-step frame k)) ...)))))))))))

(define-syntax %op-once
  (lambda (stx)
    (syntax-case stx ()
      ((_ %op ra_ ...)
       (with-syntax ([(ra ...) (generate-temporaries #'(ra_ ...))])
         #'(lambda (ra ...)
             (%op ra ...)))))))

(define-syntax %op-once-z
  (lambda (stx)
    (syntax-case stx ()
      ((_ %op ra_ ...)
       (with-syntax ([(ra ...) (generate-temporaries #'(ra_ ...))])
         #'(lambda (ra ...)
             (%op (ra (%%ra-zero ra)) ...)))))))

(define-syntax %slice-loop
  (lambda (stx)
    (syntax-case stx ()
      ((_ k_ op-once op-loop %list %let ra_ ...)
       (with-syntax ([(ra ...) (generate-temporaries #'(ra_ ...))]
                     [(frame ...) (generate-temporaries #'(ra_ ...))]
                     [(step ...) (generate-temporaries #'(ra_ ...))]
                     [(s ...) (generate-temporaries #'(ra_ ...))]
                     [(ss ...) (generate-temporaries #'(ra_ ...))]
                     [(sm ...) (generate-temporaries #'(ra_ ...))])
         #`(let ((k k_))
; create (rank(ra) - k) slices that we'll use to iterate by bumping their zeros.
             (%let ((frame ...) (ra_ ...) identity)
               (%let ((ra ...) (frame ...)
                      (lambda (ro)
                        (make-ra-raw (%%ra-data ro)
                                     (ra-pos-first (%%ra-zero ro) (vector-take (%%ra-dims ro) k))
                                     (vector-drop (%%ra-dims ro) k))))
                 (receive (los lens) (apply ra-slice-for-each-check k (%list frame ...))
; since we'll unroll, special case for rank 0
                   (if (zero? k)
; no fresh slice descriptor like in array-slice-for-each. That should be all right in newra, b/c the descriptors can be copied.
; see also below.
                     (op-once ra ...)
                     (let/ec exit
; check early so we can save a step in the loop later.
                       (vector-for-each (lambda (len) (when (zero? len) (exit))) lens)
; we'll do a normal rank-loop in [0..u) and unroll dimensions [u..k); u must be found.
; the last axis of the frame can always be unrolled, so we start checking from the one before.
                       (let ((u (- k 1)))
                         (%let ((step ...) (frame ...) (lambda (frome) (%%ra-step frome u)))
                           (receive (u len)
                               (let loop ((u u) (len 1) (s step) ...)
                                 (let ((lenu (vector-ref lens u)))
                                   (if (zero? u)
                                     (values u (* len lenu))
                                     (%let ((ss ...) (s ...) (cut * lenu <>))
                                       (%let ((sm ...) (frame ...) (lambda (frome) (%%ra-step frome (- u 1))))
                                         (if (and (equal? ss sm) ...)
                                           (loop (- u 1) (* len lenu) ss ...)
                                           (values u (* len lenu))))))))
                             (let ((lenm (- len 1)))
                               (op-loop lens lenm u ra ... frame ... step ...))))))))))))))))

(define (ra-slice-for-each-4 k op ra . rx)
  (letrec-syntax
      ((%op
        (syntax-rules ()
          ((_ ra ...) (op ra ...))))
       (%apply-op
        (syntax-rules ()
          ((_ ra) (apply op ra))))
       (%args
        (syntax-rules ()
          ((_ ra ...)
           (%slice-loop k (%op-once %op ra ...) (%op-loop %op %stepu %stepk ra ...)
                        %list %let ra ...)))))
    (apply (case-lambda
            (() (%args ra))
            ((rb) (%args ra rb))
            ((rb rc) (%args ra rb rc))
            (rx
             (let ((ry (cons ra rx)))
               (%slice-loop k (%op-once %apply-op ry) (%op-loop %apply-op %apply-stepu %apply-stepk ry)
                            %apply-list %apply-let ry))))
      rx)))

(define ra-slice-for-each ra-slice-for-each-4)


; ----------------
; special rank-0 versions, ra-for-each, ra-map!, ra-copy!, ra-equal?
; ----------------

; If op-loop takes 2 args as a rest list, here we must do that as well.
(define slice-loop-fun
  (case-lambda
   ((op-once op-loop r0)
    (%slice-loop (ra-rank r0) op-once op-loop %list %let r0))
   ((op-once op-loop r0 r1)
    (%slice-loop (ra-rank r0) op-once op-loop %list %let r0 r1))
   ((op-once op-loop r0 r1 r2)
    (%slice-loop (ra-rank r0) op-once op-loop %list %let r0 r1 r2))
   ((op-once op-loop . r)
    (%slice-loop (ra-rank (car r)) op-once op-loop %apply-list %apply-let r))))

(define-syntax %default
  (syntax-rules ()
    ((_ %op ra ...)
     (slice-loop-fun (%op-once-z %op ra ...)
                     (%op-loop-z %op ra ...)
                     ra ...))))

(define-syntax %apply-default
  (syntax-rules ()
    ((_ %apply-op ra)
     (apply slice-loop-fun
       (%op-once %apply-op ra)
       (%op-loop %apply-op %apply-stepu %apply-stepk ra)
       ra))))

(define-syntax %%subop
  (lambda (stx)
    (syntax-case stx ()
      ((_ %op (vref-ra vset!-ra ra) ...)
       (with-syntax ([(z ...) (generate-temporaries #'(ra ...))])
         #'(let-syntax
               ((%op-op
                 (syntax-rules ()
                   ((_ (ra z) ...)
                    (%op (vref-ra vset!-ra ra z) ...)))))
             (%default %op-op ra ...)))))))

; FIXME Zero, one, infinity :-|
; FIXME Partial dispatch? i.e. the first type is supported but not the others.
; FIXME Compile cases on demand.
(define-syntax %dispatch
  (lambda (stx)
    (syntax-case stx ()
      ((_ %typed-op %op ra)
       #`(case (ra-type ra)
           #,@(map (match-lambda
                     ((tag-ra vref-ra vset!-ra)
                      #`((#,tag-ra)
                         (%%subop %typed-op
                                  (#,vref-ra #,vset!-ra ra)))))
                syntax-accessors-1)
           (else (%default %op ra))))
      ((_ %typed-op %op ra rb)
       #`(case (ra-type ra)
           #,@(map (match-lambda
                     ((tag-ra vref-ra vset!-ra)
                      #`((#,tag-ra)
                         (case (ra-type rb)
                           #,@(map (match-lambda
                                     ((tag-rb vref-rb vset!-rb)
                                      #`((#,tag-rb)
                                         (%%subop %typed-op
                                                  (#,vref-ra #,vset!-ra ra)
                                                  (#,vref-rb #,vset!-rb rb)))))
                                syntax-accessors-2)
                           (else (%default %op ra rb)))
                         rb)))
                syntax-accessors-2)
           (else (%default %op ra rb))))
      ((_ %typed-op %op ra rb rc)
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
                                                         (%%subop %typed-op
                                                                  (#,vref-ra #,vset!-ra ra)
                                                                  (#,vref-rb #,vset!-rb rb)
                                                                  (#,vref-rc #,vset!-rc rc)))))
                                                syntax-accessors-3)
                                           (else (%default %op ra rb rc))))))
                                syntax-accessors-3)
                           (else (%default %op ra rb rc)))
                         rb)))
                syntax-accessors-3)
           (else (%default %op ra rb rc)))))))

(define (ra-for-each op ra . rx)
  "ra-for-each op rx ...

   Apply op to each tuple of elements from ras RX ..."
  (let-syntax
      ((%typed-fe
        (syntax-rules ()
          ((_ (vref-ra vset!-ra ra za) ...)
           (op (vref-ra (%%ra-data ra) za) ...))))
       (%fe
        (syntax-rules ()
          ((_ (ra za) ...)
           (op ((%%ra-vref ra) (%%ra-data ra) za) ...))))
       (%apply-fe
        (syntax-rules ()
          ((_ rx)
           (apply op (map (lambda (ra) ((%%ra-vref ra) (%%ra-data ra) (%%ra-zero ra))) rx))))))
    (apply (case-lambda
            (() (%dispatch %typed-fe %fe ra))
            ((rb) (%dispatch %typed-fe %fe ra rb))
            ((rb rc) (%dispatch %typed-fe %fe ra rb rc))
            (rx (%apply-default %apply-fe (cons ra rx))))
      rx)))

(define (ra-map! ra op . rx)
  "ra-map! ra op rx ...

   Apply op to each tuple of elements from ras RX ... and store the result in
   the matching position of ra RA. All the RX .. must have the same shape as RA.

   Returns the updated ra RA."
  (let-syntax
      ((%typed-map!
        (syntax-rules ()
          ((_ (vref-ra vset!-ra ra za) (vref-rx vset!-rx rx zx) ...)
           (vset!-ra (%%ra-data ra) za
                     (op (vref-rx (%%ra-data rx) zx) ...)))))
       (%map!
        (syntax-rules ()
          ((_ (ra za) (rx zx) ...)
           ((%%ra-vset! ra) (%%ra-data ra) za
            (op ((%%ra-vref rx) (%%ra-data rx) zx) ...)))))
       (%apply-map!
        (syntax-rules ()
          ((_ rx)
           ((%%ra-vset! (car rx)) (%%ra-data (car rx)) (%%ra-zero (car rx))
            (apply op (map (lambda (ra) ((%%ra-vref ra) (%%ra-data ra) (%%ra-zero ra))) (cdr rx))))))))
    (apply (case-lambda
            (() (%dispatch %typed-map! %map! ra))
            ((rb) (%dispatch %typed-map! %map! ra rb))
            ((rb rc) (%dispatch %typed-map! %map! ra rb rc))
            (rx (%apply-default %apply-map! (cons ra rx))))
      rx)
    ra))

(define (ra-fill! ra fill)
  "ra-fill! ra fill

   Fill ra RA with value FILL. RA must be of a type compatible with FILL.

   This function returns the filled ra RA."
  (let-syntax
      ((%typed-fill!
        (syntax-rules ()
          ((_ (vref-ra vset!-ra ra za))
           (vset!-ra (%%ra-data ra) za fill))))
       (%fill!
        (syntax-rules ()
          ((_ (ra za))
           ((%%ra-vset! ra) (%%ra-data ra) za fill)))))
    (%dispatch %typed-fill! %fill! ra)
    ra))

(define (ra-copy! ra rb)
  "ra-copy! ra rb

   Copy the contents of ra RA into ra RB. RA and RB must have the same shape and
   be of compatible types.

   This function returns the updated ra RB."
  (let-syntax
      ((%typed-copy!
        (syntax-rules ()
          ((_ (vref-ra vset!-ra ra za) (vref-rb vset!-rb rb zb))
           (vset!-rb (%%ra-data rb) zb
                     (vref-ra (%%ra-data ra) za)))))
       (%copy!
        (syntax-rules ()
          ((_ (ra za) (rb zb))
           ((%%ra-vset! rb) (%%ra-data rb) zb
            ((%%ra-vref ra) (%%ra-data ra) za))))))
    (%dispatch %typed-copy! %copy! ra rb)
    rb))

(define (ra-equal? . rx)
  "ra-equal? rx ...

   Return #t if the ras RX ... have the same shape and all the elements are
   EQUAL? between them, or #f otherwise."
  (let/ec exit
    (let-syntax
        ((%typed-equal?
          (syntax-rules ()
            ((_ (vref-ra vset!-ra ra za) ...)
             (unless (equal? (vref-ra (%%ra-data ra) za) ...)
               (exit #f)))))
         (%equal?
          (syntax-rules ()
            ((_ (ra za) ...)
             (unless (equal? ((%%ra-vref ra) (%%ra-data ra) za) ...)
               (exit #f))))))
      (or (null? rx)
          (null? (cdr rx))
          (let ((da (ra-dims (car rx)))
                (ta (ra-type (car rx))))
            (for-each (lambda (rb)
                        (unless (eq? ta (ra-type rb))
                          (exit #f))
                        (vector-for-each
                         (lambda (da db)
                           (unless (and (= (dim-lo da) (dim-lo db)) (= (dim-len da) (dim-len db)))
                             (exit #f)))
                         da (ra-dims rb)))
                      (cdr rx))
            (apply (case-lambda
                    ((ra rb) (%dispatch %typed-equal? %equal? ra rb))
                    (rx (apply ra-for-each (lambda x (unless (apply equal? x) (exit #f))) rx)))
              rx)
            #t)))))
