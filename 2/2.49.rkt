#lang racket

(require sicp-pict)

(define ouline-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 0.99 0))
    (make-segment (make-vect 0.99 0) (make-vect 0.99 0.99))
    (make-segment (make-vect 0.99 0.99) (make-vect 0 0.99))
    (make-segment (make-vect 0 0.99) (make-vect 0 0)))))

(define x-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 0.99 0.99))
    (make-segment (make-vect 0.99 0) (make-vect 0 0.99)))))

(define romb-painter
  (segments->painter
   (list
    (make-segment (make-vect 0.5 0) (make-vect 0.999 0.5))
    (make-segment (make-vect 0.999 0.5) (make-vect 0.5 0.999))
    (make-segment (make-vect 0.5 0.999) (make-vect 0 0.5))
    (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
    )))

(paint ouline-painter)
(paint x-painter)
(paint romb-painter)
