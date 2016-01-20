#lang racket

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment a b) (cons a b))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (mid-point s)
  (let ((a (start-segment s))
        (b (end-segment s)))
    (let ((x1 (x-point a))
          (x2 (x-point b))
          (y1 (y-point a))
          (y2 (y-point b)))
      (make-point
       (/ (+ x1 x2) 2)
       (/ (+ y1 y2) 2)))))

(mid-point
 (make-segment
  (make-point 1 1)
  (make-point 3 3)))