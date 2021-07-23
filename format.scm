
; format-ra sandbox.
; TODO when done, move to newra/mod/newra/lib/format
; FIXME broken with any length zero
; FIXME crossings and corners

(import (newra newra) (srfi :1) (srfi :26) (srfi :71) (ice-9 match))

(define (make-sc d0 d1)
  (let* ((o (char->integer #\0))
         (d (- (char->integer #\z) o)))
    (ra-map! (make-typed-ra 'a #\space d0 d1)
             (lambda (i) #\space ;; #\░;; (integer->char (+ o (remainder i d)))
                     )
             (ra-i d0 d1))))

(define* (sc-print sc #:optional o)
  (let ((o (match o
             (#t (current-output-port))
             (#f (throw 'bad-output-spec))
             (o o))))
    (ra-slice-for-each 1
      (lambda (line)
        (ra-for-each (cut display <> o) line)
        (newline o))
      sc)))


; -------------------
; example
; -------------------

(define art-0 "│─┌┐└┘├┤┬┴┼")
(define art-1 "║═╔╗╚╝╠╣╦╩╬")
(define art-2 "┃━┏┓┗┛┣┫┳┻╋")
(define art-3 "████████████")
(define art-x "-|+++++++++")
(define arts (make-ra-root (vector art-0 art-1 art-2 art-3)))

(define (ra-format o ra fmt)
; size the cells FIXME handle nested ra once the whole routine is working
  (define s (ra-map! (apply make-ra #f (ra-dimensions ra))
                     (lambda (x)
                       (if (ra? x)
                         (ra-format #f x fmt)
                         (ra-tile (array->ra (format #f fmt x)) 1))
                       )
                     ra))
  (define-values (dim-tall dim-wide)
    (let* ((q r (euclidean/ (ra-rank s) 2))
           (a (ra-iota (+ q r) 0 2))
           (b (ra-iota q 1 2)))
      (if (zero? r)
        (values a b)
        (values b a))))
  (define (widths dim0 dim1 k)
    (let* ((sq (apply ra-untranspose s (ra->list (ra-pcat #f 0 dim1 dim0))))
           (d (apply make-ra 0 (drop (ra-dimensions sq) (ra-len dim1)))))
      (ra-slice-for-each-in-order (ra-len dim1)
                                  (lambda (w)
                                    (ra-map! d (lambda (d w) (max d (+ 1 (ra-len w k)))) d w))
                                  sq)
      d))
  (define d0 (widths dim-tall dim-wide 0))
  (define d1 (widths dim-wide dim-tall 1))
  (define t0 (ra-fold + 0 d0))
  (define t1 (ra-fold + 0 d1))
  (define (line-0 sc z range at) (ra-amend! sc (string-ref (ra-ref arts (- (ra-rank z) 1)) 0) range at))
  (define (line-1 sc z range at) (ra-amend! sc (string-ref (ra-ref arts (- (ra-rank z) 1)) 1) at range))
  (define (grid1 sc z p line t0 t1)
    (when (> (ra-rank z) 0)
      (line sc z (ra-iota (- t0 1) 1) 0)
      (line sc z (ra-iota (- t0 1) 1) t1))
    (let loop ((z z) (p p))
      (if (zero? (ra-rank z))
        (+ p (z))
        (begin
          (if (> (ra-len z) 0)
; FIXME use regular iteration, else have ra-first ra-rest
            (let* ((dim0 (vector-ref (ra-dims z) 0))
                   (lo (dim-lo dim0))
                   (hi (dim-hi dim0))
                   (p (loop (ra-from z lo) p)))
              (when (> (ra-len z) 1)
                (line sc z (ra-iota (- t0 1) 1) p))
              (loop (ra-from z (ra-iota (- hi lo) (+ lo 1))) p))
            p)))))
  (define (scan-0 ra)
    (let ((c (ra-copy ra))
          (s 0))
      (ra-map! c (lambda (c) (let ((z s)) (set! s (+ s c)) z)) c)))

; make screen and print

  (define sc (make-sc (+ 1 t0) (+ 1 t1)))
  (grid1 sc d0 0 line-1 t1 t0)
  (grid1 sc d1 0 line-0 t0 t1)
; FIXME this only requires 2 dims, so avoid reshapes
  (ra-for-each
   (lambda (sq o0 d0 o1 d1)
     (ra-copy! (ra-from sc
                        (ra-iota (ra-len sq 0) (+ o0 1))
                        (ra-iota (ra-len sq 1) (+ o1 1 (- d1 (ra-len sq 1) 1)))) ; align right
               sq)
     )
   (apply ra-untranspose s (ra->list (ra-pcat #f 0 dim-tall dim-wide)))
   (apply ra-reshape (scan-0 (ra-ravel d0)) (ra-dimensions d0))
   d0
   (ra-transpose (apply ra-reshape (scan-0 (ra-ravel d1)) (ra-dimensions d1)) (ra-rank d0))
   (ra-transpose d1 (ra-rank d0)))
  (if o
    (sc-print sc o)
    sc))

(define ra (ra-i 1 8 9 3))
(define ra (ra-map! (ra-copy #t ra) sqrt ra))
(ra-format #t ra "~4,2f")

(define ra (ra-set! (ra-set! (ra-copy #t (ra-i 3 4 3))
                             (ra-i 2 3) 1 2 1)
                    (make-ra-root #(hello world of ras) (c-dims 2 2))
                    2 1 2))
(ra-format #t ra "~a")

(define ra (ra-i 2 2 2 2 2 2 2 2))
(ra-format #t ra "~a")

(define ra (ra-tile-right (ra-set! (ra-copy #t (ra-i 7)) (make-ra-root #(hello world of ras) (c-dims 2 2)) 3) 1))
(ra-format #t ra "~a")

; example from srfi-164
(define arr
   #2@1:2@1:3((#2((1 2) (3 4)) 9 #2((3 4) (5 6)))
              (#(42 43) #2((8 7 6)) #2((90 91) (100 101)))))
(define ra (array->ra arr))
(define ra (ra-map! (ra-copy ra) (lambda (x) (if (array? x) (array->ra x) x)) ra))
(ra-format #t ra "~a")

(define ra (array->ra #2@1:2@1:3((1 2 3) (4 5 6))))
(ra-format #t ra "~a")

;; (define ra (make-ra 'zero))
;; (ra-format #t ra "~a")
