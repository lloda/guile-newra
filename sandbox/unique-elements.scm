; -*- mode: scheme; coding: utf-8 -*-
; Dependency sandbox, actually

#|
Pugh1992
Eigenmann2017
https://github.com/numpy/numpy/blob/main/numpy/core/src/common/mem_overlap.c
https://github.com/Kraks/omega/blob/master/src/main/scala/omega/Omega.scala
|#

(import (only (srfi srfi-43) vector-copy! vector-fill! vector-every vector-any)
        (only (rnrs base) vector-map vector-for-each)
        (ice-9 control)
        (srfi srfi-26)
        (newra))

(define (ra-unique-elements-brute-force? ra)
  (ra-fill! ra 0)
  (let/ec exit
    (ra-slice-for-each-in-order (ra-rank ra)
      (lambda (x)
        (if (zero? (x))
          (set! (x) 1)
          (exit #f)))
      ra)
    #t))

(define (ra-unique-elements? ra)
  "
Return @code{#t} if every combination of valid indices for the array @var{ra} yields a
different element in @var{ra}'s root, otherwise return @code{#f}.
"
  (let* ((dims (%%ra-dims ra)))
    (case (vector-length dims)
      ((0) #t)
      ((1) (let* ((dim (vector-ref dims 0))
                  (len (dim-len dim)))
             (not (and (zero? (dim-step dim)) (or (not len) (> len 1))))))
      (else
       (let/ec return
; cheap cases
         (let ((sdims (sort dims (lambda (d0 d1)  (< (magnitude (dim-step d0)) (magnitude (dim-step d1)))))))
           (let loop ((i 0) (c 1) (jump-over? #t))
             (if (< i (vector-length dims))
               (let* ((dim (vector-ref dims i))
                      (len (dim-len dim)))
; len = 0
                 (cond ((and=> len zero?)
                        (return #t))
; step =0, len >1
                       ((and (zero? (dim-step dim)) (or (not len) (> len 1)))
                        (return #f))
                       (else
                        (loop (+ i 1)
                              (* c len)
; must hold for every two consecutive axes
                              (and jump-over?
                                   (or (>= (+ i 1) (vector-length dims))
                                       (let ((step (magnitude (dim-step (vector-ref sdims (+ i 1)))))
                                             (bdim (vector-ref sdims i)))
                                         (> step
                                            (* (magnitude (dim-step bdim))
                                               (max (magnitude (dim-hi bdim))
                                                    (magnitude (dim-lo bdim))))))))))))
; size <=1
               (cond ((<= c 1) (return #t))
; long step axes jump over short step axes
                     (jump-over? (return #t))
; expensive case
                     (else
; we have to solve
                      (throw 'not-yet)))))))))))

(import (srfi 64))
(test-begin "unique")

(test-begin "cheap cases")
(test-assert (ra-unique-elements? (ra-transpose (ra-copy (ra-i 4 5)) 0 0)))
(test-assert (ra-unique-elements? (ra-transpose (ra-copy (ra-i 3 4 5)) 2 1 0)))
(test-assert (ra-unique-elements? (ra-copy (ra-i 3 4 5))))
(test-assert (not (ra-unique-elements? (ra-tile (ra-copy (ra-i 3 4 5)) 0 2))))
(test-assert (ra-unique-elements? (ra-tile (ra-copy (ra-i 3 4 5)) 0 1)))
(test-assert (ra-unique-elements? (ra-from (ra-tile (ra-copy (ra-i 3 4)) 0 2) (make-ra 0) (make-ra 1) (make-ra 2))))
(test-assert (ra-unique-elements? (make-ra 0 '(1 3) '(-9 20))))
(test-assert (ra-unique-elements? (ra-reverse (make-ra 0 '(1 3) '(-9 20)) 0)))
(test-assert (ra-unique-elements? (ra-reverse (make-ra 0 '(1 3) '(-9 20)) 1)))
(test-assert (ra-unique-elements? (ra-reverse (make-ra 0 '(1 3) '(-9 20)) 0 1)))
(test-assert
 (catch 'not-yet
   (lambda ()
     (ra-unique-elements? (make-ra-root (ra-root (ra-copy #t (ra-i 10))) (vector (make-dim 3 3 2) (make-dim 3 3 1))))
     #f)
   (const #t)))

(test-end "cheap cases")

(test-end "unique")
