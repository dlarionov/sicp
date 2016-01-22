#lang racket

(define zero (lambda(f) (lambda (x) x)))

(define (inc n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (inc zero))
(define two (inc one))
(define three (inc two))
(define four (inc three))

(define (square x) (* x x))

((zero square) 2)
((one square) 2)
((two square) 2)
((three square) 2)
((four square) 2)

(define (add n m)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(((add one one) square) 2)
(((add one two) square) 2)
(((add one three) square) 2)