#lang racket

(define (repeat f times)
  (define (iter i g)
    (if (> i times)
        g
        (iter (+ 1 i) (f g))))
  (iter 0 (lambda (x) (f x))))

(define (inc x) (+ 1 x))

((repeat inc 2) 0)
  