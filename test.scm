
; Replacement for Guile C-based array system - WIP
; (c) Daniel Llorens - 2016-2017

(import (newra newra))

(make-ra (make-vector 10 3) 0 (vector (make-dim 3)))
(make-ra (make-vector 10 3) 0 (vector (make-dim 3) (make-dim 3)))
