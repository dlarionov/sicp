#lang racket

(define (make-vector x y) (cons x y))
(define (xcor v) (car v))
(define (ycor v) (cdr v))

(define (add a b)
  (make-vector
   (+ (xcor a) (xcor b))
   (+ (ycor a) (ycor b))))

(define (sub a b)
  (make-vector
   (- (xcor a) (xcor b))
   (- (ycor a) (ycor b))))

(define (scale a x)
  (make-vector
   (* (xcor a) x)
   (* (ycor a) x)))

(define a (make-vector 1 1))
(define b (make-vector 0 1))

(add a b)
(sub a b)
(scale a 3)