
; Replacement for Guile C-based array system
; (c) Daniel Llorens - 2016

(import (srfi srfi-9) (srfi srfi-9 gnu) (srfi srfi-1) (srfi srfi-8) (srfi srfi-4 gnu) (srfi srfi-26))

(define-immutable-record-type <dim>
  (make-dim size lo step) dim?
  (size dim-size)
  (lo dim-lo)
  (step dim-step))

(define (dim-end dim)
  (+ (dim-lo dim) (dim-size dim)))

; @TODO Test
(define (dim-ref dim i)
  (unless (and (<= (dim-lo dim) i) (< i (dim-end dim)))
    (throw 'dim-ref-out-of-range dim i))
  (+ (dim-lo dim) (* (dim-step dim) i)))

; @TODO Any (f linear-index) as data/vref/vset!
(define-immutable-record-type <ra>
  (make-ra* data base dims vref vset! data-len) ra?
  (data ra-data)
  (base ra-base)
  (dims ra-dims)  
  (vref ra-vref)
  (vset! ra-vset)
  (data-len ra-data-len)
)

(define (pick-typed-vector-functions v)
  (cond ((c64vector? v) (values c64vector-ref c64vector-set!                  c64vector-length)) 
        ((c32vector? v) (values c32vector-ref c32vector-set!                  c32vector-length)) 
        ((f64vector? v) (values f64vector-ref f64vector-set!                  f64vector-length)) 
        ((f32vector? v) (values f32vector-ref f32vector-set!                  f32vector-length)) 
        ((s64vector? v) (values s64vector-ref s64vector-set!                  s64vector-length)) 
        ((s32vector? v) (values s32vector-ref s32vector-set!                  s32vector-length)) 
        ((s16vector? v) (values s16vector-ref s16vector-set!                  s16vector-length)) 
        ((s8vector? v)  (values s8vector-ref  s8vector-set!                   s8vector-length))  
        ((s64vector? v) (values s64vector-ref s64vector-set!                  s64vector-length)) 
        ((s32vector? v) (values s32vector-ref s32vector-set!                  s32vector-length)) 
        ((s16vector? v) (values s16vector-ref s16vector-set!                  s16vector-length)) 
        ((s8vector? v)  (values s8vector-ref  s8vector-set!                   s8vector-length))  
        ((vector? v)    (values vector-ref    vector-set!                     vector-length))    
        ((string? v)    (values string-ref    string-set!                     string-length))
; @TODO extend this idea to 'non-strict arrays', to a method for drag-along
        ((dim? v)       (values dim-ref       (cut throw 'noo-dim-set! <...>) dim-size))
        (else (throw 'bad-vector-base-type v))))

; @TODO check bounds here
(define (make-ra data base dims)
  (receive (vref vset! data-len) (pick-typed-vector-functions data)
    (make-ra* data base dims vref vset! data-len)))

(define (make-ra-C data sizes)
  (make-ra data 0
           (map (cut make-dim <> 0 <>)
                sizes
                (if (null? sizes)
                  '()
                  (let loop ((sizes (cdr sizes)))
                    (if (null? sizes)
                      '(1)
                      (let ((next (loop (cdr sizes))))
                        (cons (* (car sizes) (car next)) next))))))))
  
(define (ra-rank ra)
  (length (ra-dims ra)))

(define (ra-tally ra)
  (unless (positive? (ra-rank ra))
    (throw 'ra-tally-bad-rank))
  (dim-size (car (ra-dims ra))))

; @TODO Test
(define (ra-item ra i)
  (let ((dims (ra-dims ra)))
    (if (null? dims)
      (throw 'ra-item-zero-rank)
      (receive (dim0 dims+) (car+cdr dims)
        (unless (and (<= (dim-lo dim0) i)
                     (< i (dim-end dim0)))
          (throw 'ra-item-out-of-range i dim0))
        (if (null? dims+)
          ((ra-vref ra) (ra-data ra) (+ (ra-base ra) i))
          (set-fields ra
            ((ra-base) (+ (ra-base ra) (* (dim-step dim0) (- i (dim-lo dim0)))))
            ((ra-dims) dims+)))))))

(define (ra-item* ra i)
  (let ((dims (ra-dims ra)))
    (if (null? dims)
      (throw 'ra-item-zero-rank)
      (receive (dim0 dims+) (car+cdr dims)
        (unless (and (<= (dim-lo dim0) i)
                     (< i (dim-end dim0)))
          (throw 'ra-item-out-of-range i dim0))
        (set-fields ra
          ((ra-base) (+ (ra-base ra) (* (dim-step dim0) (- i (dim-lo dim0)))))
          ((ra-dims) dims+))))))

; @TODO Test
; @BUG (ra-cell ra) returns ra when it should peel zero-ranks.
(define (ra-cell ra . i)
  (fold (lambda (i ra) (ra-item ra i)) ra i))

(define (ra-amend! ra rb . i)
  (let ((cell (fold (lambda (i ra) (ra-item* ra i)) ra i)))
    (if (null? (cdr (ra-dims cell)))
      ((ra-vset ra) cell (ra-base cell) (ra-cell rb))
      (throw 'bad-ra-amend ra rb i))))

; @TODO Test
(define (ra-first-cell ra k)
  (when (negative? k) (throw 'bad-frame-rank k))
  (let loop ((ra ra) (k k))
    (if (positive? k)
      (loop (ra-item ra (dim-lo (car (ra-dims ra)))) (- k 1))
      ra)))

(define (ra-for-each-cell-check k . ra)
  (let* ((dims (map (compose (cut take <> k) ra-dims) ra))
         (lo (map dim-lo (car dims)))
         (end (map dim-end (car dims))))
    (unless (every (lambda (rb) (equal? lo (map dim-lo rb))) (cdr dims))
      (throw 'mismatched-lower-bounds))
    (unless (every (lambda (rb) (equal? end (map dim-end rb))) (cdr dims))
      (throw 'mismatched-ends))
    (values lo end)))

; @TODO Unraveling version
(define (ra-for-each-cell-naive k op . ra)
  (receive (los ends) (apply ra-for-each-cell-check k ra)
; we pick a (-k)-cell for each ra and then just move along.
    (let loop-rank ((k k) (ra ra) (los los) (ends ends))
      (if (= k 0)
        (apply op ra)
        (let  ((lo (car los))
               (end (car ends))
               (steps (map (compose dim-step car ra-dims) ra)))
          (let loop-dim ((i lo))
            (unless (= i end)
              (let ((rai (map (cut ra-item* <> i) ra)))
                (loop-rank (- k 1) rai (cdr los) (cdr ends))
                (loop-dim (+ i 1))))))))))

(define (ra-for-each op ra0 . ra)
  (apply ra-for-each-cell-naive (ra-rank ra0) (lambda ra (format! "ra: ~a" ra) (apply op (map ra-cell ra))) ra0 ra))

;; (define (ra-for-each-cell k op . ra)
;;   (let ((lo (map (lambda (ra) (dim-lo (car (ra-dims ra))))
;;         (it (map ra-first-cell ra k)))
  
; @TODO Easy constructors (row-major, etc).

; --------------------
; These for examples / tests
; --------------------

(define ra0 (make-ra #(1 2 3 4 5 6) 0 (list (make-dim 2 0 3) (make-dim 3 0 1))))
(define ra1 (make-ra #(1 2 3 4 5 6) 0 (list (make-dim 3 0 2) (make-dim 2 0 1))))
(define ra2 (make-ra #(1 2 3 4 5 6) 0 (list (make-dim 0 0 1))))
(define ra3 (make-ra-C #(1 2 3 4 5 6) '(2 3)))
(define ra4 (make-ra-C #(1 2 3 4 5 6) '(3 2)))

(define rax (make-ra-C #(1 2 3 4 5 6) '(6)))
(define ray (make-ra-C #(1 2 3 4 5 6) '(6)))

(ra-for-each-cell-check 0 ra1 ra4)
(ra-for-each-cell-naive 2 (cut format! "A: ~a B: ~a" <> <>) ra1 ra4)
(ra-for-each-cell-naive 1 (cut format! "A: ~a B: ~a" <> <>) rax ray)
(ra-for-each (cut format! "A: ~a B: ~a" <> <>) rax ray)
