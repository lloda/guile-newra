
; Replacement for Guile C-based array system - Main types
; (c) Daniel Llorens - 2016-2017

; This library is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3 of the License, or (at your option) any
; later version.

(define-module (newra newra)
  #:export (make-ra ra? ra-data ra-zero ra-dims ra-vlen ra-vref ra-vset!
            make-dim dim? dim-len dim-lo dim-step dim-ref
            ra-rank ra-type
            make-ra-new make-ra-data
            array->ra ra->array
            ra-pos ra-pos-first ra-pos-lo ra-pos-hi
            ra-slice ra-cell ra-ref ra-set!
            ra-slice-for-each
            ra-slice-for-each-1 ra-slice-for-each-2 ra-slice-for-each-3))

(import (srfi srfi-9) (srfi srfi-9 gnu) (only (srfi srfi-1) fold every) (srfi srfi-8)
        (srfi srfi-4 gnu) (srfi srfi-26) (ice-9 match)
        (only (rnrs base) vector-map vector-for-each))

; ----------------
;; Glossary
; ----------------

;; dim:         each axis of an ra, or its bounds, as many as the rank.
;; index:       into an axis.
;; lo:          lowest index in a dim
;; hi:          highest index in a dim
;; end:         one past the highest index
;; len:         length of a dim = end-lo
;; v:           a vector
;; l:           a list
;; i, j:        indices in a dim, from hi to lo
;; k:           an index in a dim vector, from 0 to rank-1
;; slice:       an ra, as a piece of another ra
;; cell:        (also prefix-cell) slice obtained by fixing the first k indices into an ra.
;; item:        slice obtained by fixing the first index into an ra; a (rank - 1)-cell.

; ----------------
; misc
; ----------------

(define (struct-length s)
  (* 1/2 (string-length (symbol->string (struct-layout s)))))

(define (struct->vector s)
  (let* ((v (make-vector (struct-length s))))
    (array-index-map! v (lambda (i) (struct-ref s i)))
    v))

(define vector-fold
  (case-lambda
   ((kons knil)
    (throw 'missing-arguments))
   ((kons knil v)
    (let ((end (vector-length v)))
      (let loop ((i 0) (k knil))
        (if (= i end)
          k
          (loop (+ i 1) (kons (vector-ref v i) k))))))
   ((kons knil . vs)
    (let ((end (vector-length (car vs))))
      (let loop ((i 0) (k knil))
        (if (= i end)
          k
          (loop (+ i 1) (apply kons (append (map (cut vector-ref <> i) vs) (list k))))))))))

; FIXME ev. used newra shared, will be simpler.
(define (vector-clip v lo end)
  (unless (and (<= 0 lo end) (<= end (vector-length v)))
    (throw 'bad-arguments lo end (vector-length v)))
  (let ((w (make-vector (- end lo) *unspecified*)))
    (let loop ((i lo))
      (if (= i end)
        w
        (begin
          (vector-set! w (- i lo) (vector-ref v i))
          (loop (+ i 1)))))))

(define (vector-drop v n)
  (vector-clip v n (vector-length v)))

(define (vector-take v n)
  (vector-clip v 0 n))

; ----------------
; dimension record, used both as that, and as delayed iota.
; ----------------

(define-immutable-record-type <dim>
  (make-dim* len lo step) dim?
  (len dim-len)
  (lo dim-lo)
  (step dim-step))

(define make-dim
  (case-lambda
   ((len) (make-dim* len 0 1))
   ((len lo) (make-dim* len lo 1))
   ((len lo step) (make-dim* len lo step))))

(define (dim-end dim)
  (+ (dim-lo dim) (dim-len dim)))

(define (dim-hi dim)
  (+ (dim-lo dim) (dim-len dim) -1))

(define (dim-ref dim i)
  (unless (and (<= 0 i) (< i (dim-len dim)))
    (throw 'dim-ref-out-of-range dim i))
  (+ (dim-lo dim) (* (dim-step dim) i)))

(define (dim-check dim i)
  (unless (and (<= (dim-lo dim) i) (< i (dim-end dim)))
    (throw 'dim-check-out-of-range dim i))
  i)

; ----------------
; array type
; ----------------

(define <ra-vtable>
  (let ((v (make-struct/no-tail
            <applicable-struct-with-setter-vtable>
            (make-struct-layout (string-append "pwpw" "pwpwpwpwpwpwpw")))))
    v))

(define-inlinable (ra? o)
  (and (struct? o) (eq? <ra-vtable> (struct-vtable o))))

(define-inlinable (check-ra o)
  (unless (ra? o) (throw 'bad-ra o)))

(define (make-ra* data zero dims type vlen vref vset!)
  (letrec ((ra
            (make-struct/no-tail
             <ra-vtable>
             (lambda i (apply ra-cell ra i))
             (match-lambda* ((i ... o) (apply ra-set! ra o i))) ; it should be easier :-/
             data zero dims type vlen vref vset!)))
    ra))


;; data:    a container (function) addressable by a single integer
;; address: into data.
;; zero:    address that corresponds to all the ra indices = 0.

(define-syntax rastruct-ref (syntax-rules () ((_ a n) (begin (check-ra a) (struct-ref a n)))))
(define-syntax rastruct-set! (syntax-rules () ((_ a n o) (begin (check-ra a) (struct-set! a n o)))))

(define-inlinable (%ra-data a) (rastruct-ref a 2))
(define-inlinable (%ra-zero a) (rastruct-ref a 3))
(define-inlinable (%ra-dims a) (rastruct-ref a 4))
(define-inlinable (%ra-type a) (rastruct-ref a 5))
(define-inlinable (%ra-vlen a) (rastruct-ref a 6))
(define-inlinable (%ra-vref a) (rastruct-ref a 7))
(define-inlinable (%ra-vset! a) (rastruct-ref a 8))
; set on iteration. FIXME immutable record?
(define-inlinable (%ra-zero-set! a z) (rastruct-set! a 3 z))

(define (ra-data a) (%ra-data a))
(define (ra-zero a) (%ra-zero a))
(define (ra-dims a) (%ra-dims a))
(define (ra-type a) (%ra-type a))
(define (ra-vlen a) (%ra-vlen a))
(define (ra-vref a) (%ra-vref a))
(define (ra-vset! a) (%ra-vset! a))
; set on iteration. FIXME immutable record?
(define (ra-zero-set! a z) (%ra-zero-set! a z))

(define (pick-typed-vector-functions v)
  (cond ((vector? v)    (values  #t    vector-length     vector-ref     vector-set!   ))
        ((c64vector? v) (values  'c64  c64vector-length  c64vector-ref  c64vector-set!))
        ((c32vector? v) (values  'c32  c32vector-length  c32vector-ref  c32vector-set!))
        ((f64vector? v) (values  'f64  f64vector-length  f64vector-ref  f64vector-set!))
        ((f32vector? v) (values  'f32  f32vector-length  f32vector-ref  f32vector-set!))
        ((s64vector? v) (values  's64  s64vector-length  s64vector-ref  s64vector-set!))
        ((s32vector? v) (values  's32  s32vector-length  s32vector-ref  s32vector-set!))
        ((s16vector? v) (values  's16  s16vector-length  s16vector-ref  s16vector-set!))
        ((s8vector? v)  (values  's8   s8vector-length   s8vector-ref   s8vector-set! ))
        ((u64vector? v) (values  'u64  s64vector-length  s64vector-ref  s64vector-set!))
        ((u32vector? v) (values  'u32  s32vector-length  s32vector-ref  s32vector-set!))
        ((u16vector? v) (values  'u16  s16vector-length  s16vector-ref  s16vector-set!))
        ((u8vector? v)  (values  'u8   s8vector-length   s8vector-ref   s8vector-set! ))
        ((string? v)    (values  'a    string-length     string-ref     string-set!   ))
        ((bitvector? v) (values  'b    bitvector-length  bitvector-ref  bitvector-set!))
; @TODO extend this idea to 'non-strict arrays' (cf Racket), to a method for drag-along
        ((dim? v)       (values  'd    dim-len           dim-ref        (cut throw 'no-dim-set! <...>)))
        (else (throw 'bad-ra-data-type v))))

; low level, for conversions
(define (make-ra data zero dims)
  (unless (vector? dims) (throw 'bad-dims dims))
  (vector-for-each (lambda (dim) (unless (dim? dim) (throw 'bad-dim dim))) dims)
; after check
  (receive (type vlen vref vset!) (pick-typed-vector-functions data)
    (make-ra* data zero dims type vlen vref vset!)))

; ----------------
; compute addresses
; ----------------

; FIXME we'll optimize these I think
(define (ra-pos zero dims . i_)
  (let loop ((i i_) (j 0) (pos zero))
    (if (null? i)
      pos
      (if (>= j (vector-length dims))
        (throw 'too-many-indices i_)
        (let ((dim (vector-ref dims j)))
          (loop (cdr i) (+ j 1) (+ pos (* (dim-check dim (car i)) (dim-step dim)))))))))

; lowest position on data.
(define (ra-pos-lo ra)
  (let ((dims (%ra-dims ra)))
    (let loop ((j (- (vector-length dims) 1)) (pos (%ra-zero ra)))
      (if (< j 0)
        pos
        (let* ((dim (vector-ref dims j))
               (step (dim-step dim)))
          (loop (- j 1) (+ pos (* step (if (positive? step) (dim-lo dim) (dim-hi dim))))))))))

; highest position on data.
(define (ra-pos-hi ra)
  (let ((dims (%ra-dims ra)))
    (let loop ((j (- (vector-length dims) 1)) (pos (%ra-zero ra)))
      (if (< j 0)
        pos
        (let* ((dim (vector-ref dims j))
               (step (dim-step dim)))
          (loop (- j 1) (+ pos (* step (if (positive? step) (dim-hi dim) (dim-lo dim))))))))))

; first position of the array (when all indices = dim-lo)
; useful for some types of loops, or to transition from Guile C arrays.
(define (ra-pos-first zero dims)
  (let loop ((j (- (vector-length dims) 1)) (pos zero))
    (if (< j 0)
      pos
      (let ((dim (vector-ref dims j)))
        (loop (- j 1) (+ pos (* (dim-lo dim) (dim-step dim))))))))

; ----------------
; ref, set!, prefix slices
; ----------------

(define (ra-rank a)
  (vector-length (%ra-dims a)))

(define ra-ref
  (case-lambda
   ((ra)
    (unless (zero? (ra-rank ra))
      (throw 'bad-number-of-indices (ra-rank ra) 0))
    ((%ra-vref ra) (%ra-data ra) (%ra-zero ra)))
   ((ra . i)
    (unless (= (ra-rank ra) (length i))
      (throw 'bad-number-of-indices (ra-rank ra) (length i)))
    ((%ra-vref ra) (%ra-data ra) (apply ra-pos (%ra-zero ra) (%ra-dims ra) i)))))

(define ra-set!
  (case-lambda
   ((ra o)
    (unless (zero? (ra-rank ra))
      (throw 'bad-number-of-indices (ra-rank ra) 0))
    ((%ra-vset! ra) (%ra-data ra) (%ra-zero ra) o))
   ((ra o . i)
    (unless (= (ra-rank ra) (length i))
      (throw 'bad-number-of-indices (ra-rank ra) (length i)))
    ((%ra-vset! ra) (%ra-data ra) (apply ra-pos (%ra-zero ra) (%ra-dims ra) i) o))))

(define (ra-slice ra . i)
  (make-ra (%ra-data ra)
           (apply ra-pos (%ra-zero ra) (%ra-dims ra) i)
           (vector-drop (%ra-dims ra) (length i))))

(define (ra-cell ra . i)
  (let ((pos (apply ra-pos (%ra-zero ra) (%ra-dims ra) i)))
    (if (= (ra-rank ra) (length i))
      ((%ra-vref ra) (%ra-data ra) pos)
      (make-ra (%ra-data ra) pos (vector-drop (%ra-dims ra) (length i))))))

; ----------------
; derived functions
; ----------------

(define (c-dims-size d)
  (let* ((dims (let loop ((d d))
                 (match d
                   (()
                    '())
                   (((lo hi))
                    (list (make-dim (- hi lo -1) lo 1)))
                   ((len)
                    (list (make-dim len 0 1)))
                   (((lo hi) . rest)
                    (let ((next (loop rest))
                          (len (- hi lo -1)))
                      (cons (make-dim len lo (* (dim-len (car next)) (dim-step (car next)))) next)))
                   ((len . rest)
                    (let ((next (loop rest)))
                      (cons (make-dim len 0 (* (dim-len (car next)) (dim-step (car next)))) next)))))))
    (values (list->vector dims)
            (fold (lambda (a c) (* c (dim-len a))) 1 dims))))

(define (make-ra-data data . d)
  (receive (dims size) (c-dims-size d)
      (make-ra data
               (- (ra-pos-first 0 dims))
               dims)))

(define (make-ra-new type value . d)
  (receive (dims size) (c-dims-size d)
      (make-ra (make-typed-array type value size)
               (- (ra-pos-first 0 dims))
               dims)))

; ----------------
; transition help
; ----------------

(define (array->ra a)
  (let ((dims (list->vector
               (map (lambda (b i)
                      (make-dim (- (cadr b) (car b) -1) (car b) i))
                    (array-shape a)
                    (shared-array-increments a)))))
    (make-ra (shared-array-root a)
             (- (shared-array-offset a) (ra-pos-first 0 dims))
             dims)))

(define (ra->array ra)
  (when (eq? 'd (%ra-type ra))
    (throw 'nonconvertible-type (%ra-type ra)))
  (apply make-shared-array (%ra-data ra)
         (lambda i (list (apply ra-pos (%ra-zero ra) (%ra-dims ra) i)))
         (vector->list
          (vector-map (lambda (dim) (list (dim-lo dim) (dim-hi dim)))
                      (%ra-dims ra)))))

; ----------------
; ra-map!, ra-copy!, etc.
; ----------------

; Unlike Guile's array-for-each, etc. this one is strict —every dimension must match.
(define (ra-slice-for-each-check k . ra)
  (unless (pair? ra)
    (throw 'missing-arguments))
  (unless (<= k (ra-rank (car ra)))
    (throw 'bad-frame-rank k (ra-rank (car ra))))
  (unless (every (lambda (rai) (= (ra-rank (car ra)) (ra-rank rai))) (cdr ra))
    (throw 'bad-ranks k (map ra-rank ra)))
  (let* ((dims (map (compose (cut vector-take <> k) %ra-dims) ra))
         (lo (vector-map dim-lo (car dims)))
         (end (vector-map dim-end (car dims))))
    (unless (every (lambda (rb) (equal? lo (vector-map dim-lo rb))) (cdr dims))
      (throw 'mismatched-lo lo))
    (unless (every (lambda (rb) (equal? end (vector-map dim-end rb))) (cdr dims))
      (throw 'mismatched-end end))
    (values lo end)))

; naive
(define (ra-slice-for-each-1 kk op . ra)
  (for-each check-ra ra)
  (receive (los ends) (apply ra-slice-for-each-check kk ra)
; we pick a (-k)-slice for each ra and then just move along.
    (let loop-rank ((k 0) (ra ra))
      (if (= k kk)
        (apply op ra)
        (let  ((lo (vector-ref los k))
               (end (vector-ref ends k)))
          (let loop-dim ((i lo))
            (unless (= i end)
              (let ((rai (map (cut ra-slice <> i) ra)))
                (loop-rank (+ k 1) rai)
                (loop-dim (+ i 1))))))))))

; moving slice
(define (ra-slice-for-each-2 kk op . ra)
  (for-each check-ra ra)
  (receive (los ends) (apply ra-slice-for-each-check kk ra)
; create (rank(ra) - k) slices that we'll use to iterate by bumping their zeros.
    (let ((frame ra)
          (ra (map (lambda (ra)
                     (make-ra (%ra-data ra)
                              (ra-pos-first (%ra-zero ra) (vector-take (%ra-dims ra) kk))
                              (vector-drop (%ra-dims ra) kk)))
                   ra)))
      (let loop-rank ((k 0))
        (if (= k kk)
; BUG doesn't pass fresh slice descriptor like array-slice-for-each does.
          (apply op ra)
          (let  ((lo (vector-ref los k))
                 (end (vector-ref ends k)))
            (let loop-dim ((i lo))
              (cond
               ((= i end)
                (for-each
                    (lambda (ra frame)
                      (let ((step (dim-step (vector-ref (%ra-dims frame) k))))
                        (%ra-zero-set! ra (+ (%ra-zero ra) (* step (- lo end))))))
                  ra frame))
               (else
                (loop-rank (+ k 1))
                (for-each
                    (lambda (ra frame)
                      (let ((step (dim-step (vector-ref (%ra-dims frame) k))))
                        (%ra-zero-set! ra (+ (%ra-zero ra) step))))
                  ra frame)
                (loop-dim (+ i 1)))))))))))

; moving slice, row-major unrolling.
(define (ra-slice-for-each-3 u op . ra)
  (for-each check-ra ra)
  (receive (los ends) (apply ra-slice-for-each-check u ra)
; create (rank(ra) - k) slices that we'll use to iterate by bumping their zeros.
    (let* ((lens (vector-map - ends los))
           (frame ra)
           (ra (map (lambda (ra)
                      (make-ra (%ra-data ra)
                               (ra-pos-first (%ra-zero ra) (vector-take (%ra-dims ra) u))
                               (vector-drop (%ra-dims ra) u)))
                    ra)))
; since we'll unroll, special case for rank 0
      (if (zero? u)
        (apply op ra)
; we'll do a normal rank-loop in [0..u) and unroll dimensions [u..k); u must be found.
; the last axis of the frame can always be unrolled, so we start checking from the one before.
        (let* ((u (- u 1))
               (s0 (map (lambda (frame) (dim-step (vector-ref (%ra-dims frame) u))) frame)))
          (receive (u len)
              (let loop ((u u) (s s0) (len 1))
                (let ((lenu (vector-ref lens u)))
                  (if (zero? u)
                    (values u (* len lenu))
                    (let* ((ss (map (cut * lenu <>) s))
                           (sm (map (lambda (frame) (dim-step (vector-ref (%ra-dims frame) (- u 1)))) frame)))
                      (if (equal? ss sm)
                        (loop (- u 1) ss (* len lenu))
                        (values u (* len lenu)))))))
            (let loop-rank ((k 0))
              (if (= k u)
; unrolled dimensions.
                (let loop ((i 0))
                  (cond ((= i len)
                         (for-each (lambda (ra step) (%ra-zero-set! ra (- (%ra-zero ra) (* step len)))) ra s0))
                        (else
; BUG doesn't pass fresh slice descriptor like array-slice-for-each does.
                         (apply op ra)
                         (for-each (lambda (ra step) (%ra-zero-set! ra (+ (%ra-zero ra) step))) ra s0)
                         (loop (+ i 1)))))
                (let  ((lo (vector-ref los k))
                       (end (vector-ref ends k)))
                  (let loop-dim ((i lo))
                    (cond
                     ((= i end)
                      (for-each
                          (lambda (ra frame)
                            (let ((step (dim-step (vector-ref (%ra-dims frame) k))))
                              (%ra-zero-set! ra (- (%ra-zero ra) (* step (- end lo))))))
                        ra frame))
                     (else
                      (loop-rank (+ k 1))
                      (for-each
                          (lambda (ra frame)
                            (let ((step (dim-step (vector-ref (%ra-dims frame) k))))
                              (%ra-zero-set! ra (+ (%ra-zero ra) step))))
                        ra frame)
                      (loop-dim (+ i 1))))))))))))))

; default
(define ra-slice-for-each ra-slice-for-each-1)
