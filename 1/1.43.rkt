#lang racket

(define (repeat f times)
  (define (iter i g)
    (if (= i times)
        g
        (iter (+ 1 i) (lambda (x) (f (g x))))))
  (iter 1 f))

((repeat (lambda (x) (* x x)) 2) 5)