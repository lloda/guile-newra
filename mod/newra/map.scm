; -*- mode: scheme; coding: utf-8 -*-

; (c) Daniel Llorens - 2016-2019, 2021-2022
; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

;;; Commentary:
;; Array iteration.
;;; Code:

(define-module (newra map)
  #:export (ra-slice-for-each ra-slice-for-each-in-order
            ra-slice-for-each-check make-ra-root-prefix
            ra-fill! ra-copy! ra-swap! ra-swap-in-order! ra-map! ra-map-in-order! ra-for-each
            ra-every ra-any ra-equal?))

(import (newra base) (newra vector) (newra blis)
        (srfi srfi-9) (srfi srfi-9 gnu) (srfi srfi-71) (srfi srfi-2) (srfi srfi-26) (srfi srfi-4 gnu)
        (ice-9 match) (ice-9 control)
        (only (srfi srfi-1) fold every)
        (only (rnrs base) vector-map vector-for-each)
        (only (srfi srfi-43) vector-copy! vector-fill! vector-every)
        (only (rnrs bytevectors) bytevector-copy! bytevector-fill! bytevector?))

(eval-when (expand load eval)
  (if have-blis? (import (ffi blis) (system foreign))))


; ----------------
; ra-slice-for-each
; ----------------

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
                    (let ((lenj0 (vector-ref len j))
                          (loj0 (vector-ref lo j)))
                      (match (vector-ref (%%ra-dims ra) j)
                        (($ <dim> lenj loj _)
                         (if lenj0
                           (begin
                             (unless (match-len? lenj0 lenj)
                               (throw 'mismatched-lens lenj0 lenj 'at-dim j))
; valid len means los must be matched. lenj0 implies loj0 (cf make-dim) so we can reuse match-len?.
                             (unless (match-len? loj0 loj)
                               (throw 'mismatched-los loj0 loj 'at-dim j)))
                           (begin
                             (vector-set! len j lenj)
                             (vector-set! lo j loj)))))))))
      ra)
    (do ((j 0 (+ j 1))) ((= j k))
      (unless (vector-ref len j) (throw 'unset-len-for-dim j len))
      (unless (vector-ref lo j) (throw 'unset-lo-for-dim j lo)))
    (values lo len)))

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
(define-syntax-rule (%let ((a (x ...) b) ...) e ...)
  (let ((a b) ...) e ...))
(define-syntax-rule (%stepu n (ra step) ...)
  (begin (%%ra-zero-set! ra (+ (%%ra-zero ra) (* n step))) ...))
(define-syntax-rule (%stepk k n (ra frame) ...)
  (begin (%%ra-zero-set! ra (+ (%%ra-zero ra) (* n (%%ra-step-prefix frame k)))) ...))

(define-syntax-rule (%apply-list a)
  a)
(define-syntax-rule (%apply-let ((a (x ...) b)) e ...)
  (let ((a (map (lambda (x ...) b) x ...))) e ...))
(define-syntax-rule (%apply-stepu n (ra step))
  (for-each (lambda (ra step) (%stepu n (ra step))) ra step))
(define-syntax-rule (%apply-stepk k n (ra frame))
  (for-each (lambda (ra frame) (%stepk k n (ra frame))) ra frame))

; Extracted from %slice-loop to be specialized for each combination of argument types.
; FIXME only %op needs to be specialized for types...
(define-syntax %op-loop
  (lambda (stx)
    (syntax-case stx ()
      ((_ %op %stepu %stepk ra_ ...)
       (with-syntax (((ra ...) (generate-temporaries #'(ra_ ...)))
                     ((frame ...) (generate-temporaries #'(ra_ ...)))
                     ((step ...) (generate-temporaries #'(ra_ ...))))
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
      ((_ k_ %op %op-loop %list %let frame ...)
       (with-syntax (((ra ...) (generate-temporaries #'(frame ...)))
                     ((step ...) (generate-temporaries #'(frame ...)))
                     ((s ...) (generate-temporaries #'(frame ...)))
                     ((ss ...) (generate-temporaries #'(frame ...)))
                     ((sm ...) (generate-temporaries #'(frame ...))))
         #`(let* ((k k_)
; create (rank(ra) - k) slices that we'll use to iterate by bumping their zeros.
                  (los lens (apply ra-slice-for-each-check k (%list frame ...))))
             (%let ((ra (frame) (make-ra-root-prefix frame k los)) ...)
; since we'll unroll, special case for rank 0
               (if (zero? k)
; no fresh slice descriptor like in array-slice-for-each. Should be all right b/c the descriptors can be copied.
                 (%op ra ...)
; check early so we can save a step in the loop later.
                 (when (vector-every positive? lens)
; we'll do a normal rank-loop in [0..u) and unroll dimensions [u..k); u must be searched.
                   (let ((u (- k 1)))
                     (%let ((step (frame) (%%ra-step-prefix frame u)) ...)
                       (let* ((u len (let loop ((u u) (len 1) (s step) ...)
                                       (let ((lenu (vector-ref lens u)))
                                         (if (zero? u)
                                           (values u (* len lenu))
                                           (%let ((ss (s) (* lenu s)) ...)
                                             (%let ((sm (frame) (%%ra-step-prefix frame (- u 1))) ...)
                                               (if (and (equal? ss sm) ...)
                                                 (loop (- u 1) (* len lenu) ss ...)
                                                 (values u (* len lenu)))))))))
                              (lenm (- len 1)))
                         (%op-loop lens lenm u ra ... frame ... step ...)))))))))))))

(define (ra-slice-for-each k op . rx)
  (let-syntax
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
      rx)
    (values)))

(define ra-slice-for-each-in-order ra-slice-for-each)


; ----------------
; special rank-0 versions, ra-for-each, ra-map!, ra-copy!, ra-equal?
; ----------------

; Variant of %op-loop that avoids updating/rolling back %%ra-zero and instead keeps indices on the stack. The improvement is somewhat unreasonable... Not worth using with rest list, however (cf branch op-loop-elems-rest-list)

(define-syntax %op-loop-elems
  (lambda (stx)
    (syntax-case stx ()
      ((_ (%op0 %op1) ra_ ...)
       (with-syntax (((ra ...) (generate-temporaries #'(ra_ ...)))
                     ((frame ...) (generate-temporaries #'(ra_ ...)))
                     ((step ...) (generate-temporaries #'(ra_ ...)))
                     ((z ...) (generate-temporaries #'(ra_ ...)))
                     ((d ...) (generate-temporaries #'(ra_ ...))))
         #'(lambda (lens lenm u ra ... frame ... step ...)
             (let ((d (%%ra-root ra)) ...)
               (let loop-rank ((k 0) (z (%%ra-zero ra)) ...)
                 (if (= k u)
                   (%op1 (+ 1 lenm) (d z step) ...)
                   (let loop-dim ((i (- (vector-ref lens k) 1)) (z z) ...)
                     (loop-rank (+ k 1) z ...)
                     (unless (zero? i)
                       (loop-dim (- i 1) (+ z (%%ra-step-prefix frame k)) ...)))))))))
      ((_ (%op0) ra_ ...)
       #'(let-syntax
             ((%op1
               (syntax-rules … ()
                 ((_ len (d z step) …)
                  (let loop ((i (- len 1)) (z z) …)
                    (%op0 (d z) …)
                    (unless (zero? i)
                      (loop (- i 1) (+ z step) …)))))))
           (%op-loop-elems (%op0 %op1) ra_ ...))))))

(define-syntax-rule (%sloop (%op0 %op ...) ra ...)
  (let-syntax
      ((%op-elems
        (syntax-rules … ()
          ((_ rx …)
            (%op0 ((%%ra-root rx) (%%ra-zero rx)) …)))))
    (%slice-loop
     (max (ra-rank ra) ...)
     %op-elems
     (%op-loop-elems (%op0 %op ...) ra ...)
     %list %let ra ...)))

(define-syntax-rule (%apply-sloop %apply-op ra)
  (%slice-loop
   (fold (lambda (a b) (max b (ra-rank a))) 0 ra)
   %apply-op
   (%op-loop %apply-op %apply-stepu %apply-stepk ra)
   %apply-list %apply-let ra))

; Use this for %op0 when there's no valid %op0. That may happen when %op1 isn't generic enough (e.g. it only works with step 1) so %sloop is used for %op1 alone.

(define-syntax-rule (%pass ra ...)
  (throw 'bad-usage ra ...))


; -------------------
; dispatch type combinations
; -------------------

; These tables are used to inline specific type combinations in %dispatch.
; We handle fewer types with 2 & 3 arguments to limit the explosion in compile time.

(eval-when (expand load eval)
  (define (pick-ref-set type)
    (case (syntax->datum type)
      ((#t)  (values #'vector-ref     #'vector-set!   ))
      ((c64) (values #'c64vector-ref  #'c64vector-set!))
      ((c32) (values #'c32vector-ref  #'c32vector-set!))
      ((f64) (values #'f64vector-ref  #'f64vector-set!))
      ((f32) (values #'f32vector-ref  #'f32vector-set!))
      ((s64) (values #'s64vector-ref  #'s64vector-set!))
      ((s32) (values #'s32vector-ref  #'s32vector-set!))
      ((s16) (values #'s16vector-ref  #'s16vector-set!))
      ((s8)  (values #'s8vector-ref   #'s8vector-set! ))
      ((u64) (values #'u64vector-ref  #'u64vector-set!))
      ((u32) (values #'u32vector-ref  #'u32vector-set!))
      ((u16) (values #'u16vector-ref  #'u16vector-set!))
      ((u8)  (values #'u8vector-ref   #'u8vector-set! ))
      ((a)   (values #'string-ref     #'string-set!   ))
      ((b)   (values #'bitvector-ref  #'bitvector-set!))
      ((d)   (values #'aseq-ref       #'(cut throw 'no-aseq-set! <...>)))
      (else (throw 'bad-ra-root-type type))))
  (define syntax-accessors
    (list (list #'#t #'f64 #'d #'u8 ;; #'f32 #'c64 #'c32 #'s64 #'s32 #'s16 #'s8 #'u64 #'u32 #'u16 #'a #'b
                )
          (list #'#t #'f64 #'d ;; #'u8 #'f32 #'c64 #'c32 #'s64 #'s32 #'s16 #'s8 #'u64 #'u32 #'u16 #'a #'b
                )
          (list #'#t #'f64 ;; #'d #'u8 #'f32 #'c64 #'c32 #'s64 #'s32 #'s16 #'s8 #'u64 #'u32 #'u16 #'a #'b
                ))))

; FIXME Compile cases on demand.

(define-syntax %dispatch
  (lambda (stx)
    (syntax-case stx ()
      ((_ %op rr ...)
       #`(letrec-syntax
             ((loop
               (lambda (sty)
                 (with-ellipsis …
                   (syntax-case sty ()
                     ((_ ((vref-ra vset!-ra ra) …) (sa …))
                      (with-syntax (((d …) (generate-temporaries #'(ra …)))
                                    ((z …) (generate-temporaries #'(ra …))))
                        #'(let-syntax
                              ((%op-op
                                (syntax-rules ()
                                  ((_ (d z) …)
                                   (%op (vref-ra vset!-ra d z) …)))))
                            (%sloop (%op-op) ra …))))
                     ((_ (%terms …) (sa sb …) ra rb …)
                      #`(case (%%ra-type ra)
                          #,@(let* ((i ;; (syntax->datum #'sa) ; n0×n1×n2 for 3 args
                                     (- (length #'(rr ...)) 1) ; n2×n2×n2 for 3 args
                                     )
                                    (tags (if (< i (length syntax-accessors)) ; handle default case indefinitely
                                            (list-ref syntax-accessors i)
                                            '())))
                               (map (match-lambda
                                      (tag (let ((vref-ra vset!-ra (pick-ref-set tag)))
                                             #`((#,tag) (loop (%terms … (#,vref-ra #,vset!-ra ra)) (sb …) rb …)))))
                                 tags))
                          (else
                           (let ((vref (%%ra-vref ra))
                                 (vset! (%%ra-vset! ra)))
                             (loop (%terms … (vref vset! ra)) (sb …) rb …))))))))))
           (loop () #,(iota (length syntax-accessors)) rr ...))))))


; -------------------
; zoo
; -------------------

(define (ra-for-each op . rx)
  "
Apply @var{op} to each tuple of elements from arrays @var{rx} ... All the
@var{rx} must have matching shapes.

This function returns unspecified values.

See also: @code{ra-map!} @code{ra-slice-for-each} @code{ra-clip}
"
  (let-syntax
      ((%op
        (syntax-rules ()
          ((_ (vref-ra vset!-ra da za) ...)
           (op (vref-ra da za) ...))))
       (%apply-op
        (syntax-rules ()
          ((_ rx)
           (apply op (map (lambda (ra) ((%%ra-vref ra) (%%ra-root ra) (%%ra-zero ra))) rx))))))
    (apply (case-lambda
            ((ra) (%dispatch %op ra))
            ((ra rb) (%dispatch %op ra rb))
            ((ra rb rc) (%dispatch %op ra rb rc))
            (rx (%apply-sloop %apply-op rx)))
      rx)
    (values)))

(define (ra-map! ra op . rx)
  "
Apply @var{op} to each tuple of elements from arrays @var{rx} ... and store the
result in the matching position of array @var{ra}. @var{ra} and all the @var{rx} ...
must have matching shapes.

Returns the updated array @var{ra}.

See also: @code{ra-for-each} @code{ra-copy!} @code{ra-fill!} @code{ra-clip}
"
  (let-syntax
      ((%op
        (syntax-rules ()
          ((_ (vref-ra vset!-ra da za) (vref-rx vset!-rx dx zx) ...)
           (vset!-ra da za (op (vref-rx dx zx) ...)))))
       (%apply-op
        (syntax-rules ()
          ((_ rx)
           ((%%ra-vset! (car rx)) (%%ra-root (car rx)) (%%ra-zero (car rx))
            (apply op (map (lambda (ra) ((%%ra-vref ra) (%%ra-root ra) (%%ra-zero ra))) (cdr rx))))))))
    (apply (case-lambda
            (() (%dispatch %op ra))
            ((rb) (%dispatch %op ra rb))
            ((rb rc) (%dispatch %op ra rb rc))
            (rx (let ((ra (cons ra rx))) (%apply-sloop %apply-op ra))))
      rx)
    ra))

(define ra-map-in-order! ra-map!)

(define (ra-fill! ra fill)
  "
Fill array @var{ra} with value @var{fill}. @var{ra} must be of a type compatible
with @var{fill}.

This function returns the filled array @var{ra}.

See also: @code{ra-copy!} @code{ra-map!}
"
  (define (line! t step)
    (match t
; These only support step 1.
      (#t
       (and (= step 1)
            (lambda (da len za stepa)
              (unless (= 1 stepa) (throw 'bad-assumption-in-ra-fill!)) ; FIXME depends on traversal order.
              (vector-fill! da fill za (+ za len)))))
      ('s
       (and (= step 1)
            (lambda (da len za stepa)
              (unless (= 1 stepa) (throw 'bad-assumption-in-ra-fill!)) ; FIXME depends on traversal order.
              (string-fill! da fill za (+ za len)))))
      ((or 'u8 's8 'vu8)
       (and (= step 1)
            (lambda (da len za stepa)
              (unless (= 1 stepa) (throw 'bad-assumption-in-ra-fill!)) ; FIXME depends on traversal order.
              (bytevector-fill! da fill za (+ za len)))))
      ((or 's32 'u32 'f32 's64 'u64 'f64 'c32 'c64)
;  FIXME needs further size heuristic, can be slower than general case when len is small. We should have len here.
       (and have-blis?
            (let ((fill* (bytevector->pointer (make-typed-array t fill 1)))
                  (fun type-size (match t
                                   ((or 's32 'u32 'f32) (values bli_ssetv 4))
                                   ((or 's64 'u64 'f64 'c32) (values bli_dsetv 8))
                                   ('c64 (values bli_zsetv 16)))))
              (lambda (da len za stepa)
                (fun BLIS_NO_CONJUGATE len fill* (bytevector->pointer da (* type-size za)) stepa)))))
      ('d (throw 'cannot-fill-type 'd))
      (t #f)))

; optimization 1
  (cond ((and (positive? (ra-rank ra))
              (and-let* ((line! (line! (%%ra-type ra) (%%ra-step ra (- (%%ra-rank ra) 1)))))
                (let-syntax
                    ((%fill!
                      (syntax-rules ()
                        ((_ len (da za stepa))
                         (line! da len za stepa))))) ; FIXME really only za is necessary.
                  (%sloop (%pass %fill!) ra)
                  ra))))
; general case
        (else
         (let-syntax
             ((%op
               (syntax-rules ()
                 ((_ (vref-ra vset!-ra da za))
                  (vset!-ra da za fill)))))
           (%dispatch %op ra)
           ra))))

(define (ra-copy! ra rb)
  "
Copy the contents of array @var{rb} into array @var{ra}. @var{ra} and @var{rb}
must have matching shapes and be of compatible types.

This function returns the updated array @var{ra}.

See also: @code{ra-fill!} @code{ra-map!} @code{ra-clip}
"
; These only support step 1.
; Or we could use bli_?copyv / bli_?copym from BLIS.
  (define (line! root)
    (cond ((vector? root)
           (lambda (dst dstart src sstart len)
             (vector-copy! dst dstart src sstart (+ sstart len))))
          ((string? root)
           (lambda (dst dstart src sstart len)
             (string-copy! dst dstart src sstart (+ sstart len))))
          ((bytevector? root)
           (let ((bs (srfi-4-vector-type-size root)))
             (lambda (dst dstart src sstart len)
               (bytevector-copy! src (* bs sstart) dst (* bs dstart) (* bs len)))))
          ((aseq? root) (throw 'cannot-copy!-type 'd))
          (else #f)))

  (let ((rankb (ra-rank rb)))
; optimization 1
    (cond ((zero? rankb)
           (ra-fill! ra (ra-ref rb)))
; optimization 2
          ((and (positive? (ra-rank ra))
                (eq? (%%ra-type ra) (%%ra-type rb))
                (= 1 (%%ra-step ra (- rankb 1)) (%%ra-step rb (- rankb 1)))
                (and-let* ((line! (line! (%%ra-root rb))))
; FIXME refactor with the general case below
                  (let-syntax
                      ((%copy!
                        (syntax-rules ()
                          ((_ len (da za stepa) (db zb stepb))
; FIXME this assumption depends on traversal order.
                           (if (= 1 stepa stepb)
                             (line! da za db zb len)
                             (throw 'bad-assumption-in-ra-copy!))))))
                    (%sloop (%pass %copy!) ra rb)
                    ra))))
; general case
          (else
           (let-syntax
               ((%op
                 (syntax-rules ()
                   ((_ (vref-ra vset!-ra da za) (vref-rb vset!-rb db zb))
                    (vset!-ra da za (vref-rb db zb))))))
             (%dispatch %op ra rb)
             ra)))))

(define (ra-swap! ra rb)
  "
Swap the contents of @var{rb} and @var{ra}. @var{ra} and @var{rb} must have
matching shapes and be of compatible types.

This function returns the swapped array @var{ra}.

See also: @code{ra-copy!} @code{ra-fill!} @code{ra-map!}
"
  (let-syntax
      ((%op
        (syntax-rules ()
          ((_ (vref-ra vset!-ra da za) (vref-rb vset!-rb db zb))
           (let ((c (vref-ra da za)))
             (vset!-ra da za (vref-rb db zb))
             (vset!-rb db zb c))))))
    (%dispatch %op ra rb)
    ra))

(define ra-swap-in-order! ra-swap!)

; FIXME refactor ra-any ra-every

(define (ra-every pred? . rx)
  "
@var{rx} must be arrays of matching shapes. Return true if @code{(pred? rxi ..)}
is true for every tuple @var{rxi} ... of matching elements of @var{rx} ...,
otherwise return @code{#f}.

See also: @code{ra-any} @code{ra-equal?} @code{ra-fold}
"
  (let/ec exit
    (let-syntax
        ((%op
          (syntax-rules ()
            ((_ (vref-ra vset!-ra da za) ...)
             (unless (pred? (vref-ra da za) ...)
               (exit #f))))))
      (or (null? rx)
          (begin
            (apply (case-lambda
                    ((ra) (%dispatch %op ra))
                    ((ra rb) (%dispatch %op ra rb))
                    ((ra rb rc) (%dispatch %op ra rb rc))
                    (rx (apply ra-for-each (lambda x (unless (apply pred? x) (exit #f))) rx)))
              rx)
            #t)))))

(define (ra-any pred? . rx)
  "
@var{rx} must be arrays of matching shapes. Return @code{{pred? rxi ..)} is that is
true for some tuple @var{rxi} ... of matching elements of @var{rx} ...,
otherwise return @code{#f}.

For example:

@lisp
; find some i, j such that A(i, j) is true
(define I (ra-iota #f))
(define J (ra-transpose (ra-iota #f) 1))
(ra-any (lambda (a i j) (and a (vector i j))) A I J)
@end lisp

See also: @code{ra-every} @code{ra-equal?} @code{ra-fold}
"
  (let/ec exit
    (let-syntax
        ((%op
          (syntax-rules ()
            ((_ (vref-ra vset!-ra da za) ...)
             (and=> (pred? (vref-ra da za) ...) exit)))))
      (or (null? rx)
          (begin
            (apply (case-lambda
                    ((ra) (%dispatch %op ra))
                    ((ra rb) (%dispatch %op ra rb))
                    ((ra rb rc) (%dispatch %op ra rb rc))
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
                     (match-lambda*
                       ((($ <dim> alen alo _) ($ <dim> blen blo _))
                        (unless (and (eqv? alo blo) (eqv? alen blen))
                          (exit #f))))
                     da db)))
        rx)
      #t)))

; FIXME it's a bit silly that (ra-transpose (ra-i 9) 1) isn't ra-equal? to
; itself because dead axes don't match each other.
; FIXME built-in array-equal? says 'equal? or array-equal?' for the elements.

(define (ra-equal? . rx)
  "
Return true if the arrays @var{rx} ... have the same shapes and types and their
corresponding elements are @code{equal?}, or #f otherwise.

See also: @code{ra-map!} @code{ra-for-each}
"
  (let/ec exit
    (let-syntax
        ((%op
          (syntax-rules ()
            ((_ (vref-ra vset!-ra da za) ...)
             (unless (equal? (vref-ra da za) ...)
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
                         ((ra rb) (%dispatch %op ra rb))
                         (rx (apply ra-for-each (lambda x (unless (apply equal? x) (exit #f))) rx)))
                   rx)
                 #t))))))
