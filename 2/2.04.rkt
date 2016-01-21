#lang racket

(define (cons x y)
  (lambda (f) (f x y)))

(define (car z)
  (z (lambda(p q) p)))

(define (cdr z)
  (z (lambda(p q) q)))

(define (make-rat x y) (cons x y))
(define (numer z) (car z))
(define (denom z) (cdr z))

(denom (make-rat 1 2))