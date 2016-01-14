#lang racket

(define (% a b) (* (/ b 100) a))

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (let [(next (improve guess))]
      (if (good-enough? guess next)
          guess
          (iter next))))
  (lambda(x) (iter x)))

(define (sqrt x)  
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2)) 
  (define (good-enough? prev next)
    (< (abs (- next prev)) (% 0.0001 prev)))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt 49)

(define (fixed-point f guess)  
  (define (improve guess)
    (f guess))
  (define (good-enough? prev next)
    (< (abs (- next prev)) (% 0.00001 prev)))
  ((iterative-improve good-enough? improve) guess))

(fixed-point (lambda (x) (* 0.5 (+ x (/ 49 x)))) 1.0)