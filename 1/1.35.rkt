#lang racket

(define tolerance 0.0001)

(define (fixed-point f guess)
  (define (try x)
    (let [(y (f x))]
      (define (good? x)
        (> tolerance (abs (- x y))))
      (if (good? x)
          y
          (try y))))
  (try guess))

(fixed-point (lambda (x) (* 0.5 (+ x (/ 49 x)))) 1.0)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
