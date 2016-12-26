#lang racket

(require sicp-pict)

(define flip-horiz
  (transform-painter
   (make-vect 1 0)
   (make-vect 0 0)
   (make-vect 1 1)))

(define rotate180
  (transform-painter
   (make-vect 1 1)
   (make-vect 0 1)
   (make-vect 1 0)))

(define rotate270
  (transform-painter
   (make-vect 0 1)
   (make-vect 0 0)
   (make-vect 1 1)))

(paint einstein)
(paint (flip-vert einstein))
(paint (flip-horiz einstein))
(paint (rotate180 einstein))
(paint (rotate270 einstein))

