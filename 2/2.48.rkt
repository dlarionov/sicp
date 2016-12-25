#lang racket

(define (make-vector x y) (cons x y))
(define (xcor v) (car v))
(define (ycor v) (cdr v))

(define (make-segment a b) (cons a b))
(define (start s) (car s))
(define (end s) (cdr s))

(define a (make-vector 0 1))
(define b (make-vector 1 0))
(define s (make-segment a b))
(start s)
(end s)
