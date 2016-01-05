#lang racket

(define (abs x)
  (cond ((< x 0) (- x))
        ((= x 0) x)
        ((> x 0) x)))

(define (avg a b)
        (/ (+ a b) 2))

(define (improve guess x)
    (avg guess (/ x guess)))

(define (good-enouth? guess x)
  (< (abs (- (* guess guess) x)) 0.01))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enouth? guess x) guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 13)



