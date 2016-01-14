#lang racket

(define (% a b) (* (/ b 100) a))
(define (avg a b) (/ (+ a b) 2))
(define (square x) (* x x))

(define (improve guess x)
  (avg guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.01))

(define (new-good-enough? prev next)
  (< (abs (- next prev)) (% 0.01 prev)))

(define (sqrt-iter guess x)
  (if (new-good-enough? (improve guess x) guess) 
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(square (sqrt 0.0006))





