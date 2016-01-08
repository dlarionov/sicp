#lang racket

(define (multiply-iterative term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (* result (term x)))))
  (iter a 1))


(define (multiply-recursive term a next b)
  (if (> a b)
      1
      (* (term a) (multiply-recursive term (next a) next b))))

(define (fib n)
  (define (next x) (+ 1 x))
  (define (term x) x)
  (multiply-recursive term 1 next n))

(fib 4)
  
(define (pi n)
  (define (next x) (+ 1 x))
  (define (term x)
    (if (even? x)
        (/ (+ 2 x) (+ 1 x))
        (/ (+ 1 x) (+ 2 x))))
  (* 4.0 (multiply-iterative term 1 next n)))


(pi 10000)