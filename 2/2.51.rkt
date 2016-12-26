#lang racket

(require sicp-pict)

(define left
  (transform-painter
   (make-vect 0 0)
   (make-vect 0.5 0)
   (make-vect 0 1)))

(define right
  (transform-painter
   (make-vect 0.5 0)
   (make-vect 1 0)
   (make-vect 0.5 1)))

(define top
  (transform-painter
   (make-vect 0 0.5)
   (make-vect 1 0.5)
   (make-vect 0 1)))

(define bottom
  (transform-painter
   (make-vect 0 0)
   (make-vect 1 0)
   (make-vect 0 0.5)))

(define (beside p1 p2)
  (lambda (frame)
    ((left p1) frame)
    ((right p2) frame)))

(define (below p1 p2)
  (lambda (frame)
    ((top p2) frame)
    ((bottom p1) frame)))

(define (below2 p1 p2)
  (rotate90 (beside (rotate270 p1) (rotate270 p2))))

(paint (beside einstein diagonal-shading))
(paint (below2 einstein diagonal-shading))