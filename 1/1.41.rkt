#lang racket

(define (double f)
  (lambda(x) (f (f x))))

(define (inc x)
  (+ 1 x))


((double inc) 0)
(((double double) inc) 0)
(((double (double double)) inc) 0)
(((double double)(double inc)) 0)