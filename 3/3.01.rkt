#lang racket

(define (make-accumulator amount)
  (lambda(x)
    (begin (set! amount (+ amount x)) amount)))

(define A (make-accumulator 100))

(A 10)
(A 42)
(A -25)