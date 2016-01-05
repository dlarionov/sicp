#lang racket

(define (avg a b)
        (/ (+ a b) 2))

(define (improve guess x)
    (avg guess (/ x guess)))

(define (square x)
  (* x x))

(define (good-enouth? guess x)
  (< (abs (- (square guess) x)) 0.01))

(define (new-good-enouth? prev next)
  (< (abs (- next prev)) (% 0.01 prev)))

(define (% a b)
  (* (/ b 100) a))

(define (sqrt-iter guess x)
  (if (new-good-enouth? (improve guess x) guess) guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(square (sqrt 0.0006))





