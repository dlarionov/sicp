#lang racket

(define (accumulate-iterative combiner null-value term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (combiner result (term x)))))
  (iter a null-value))

(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-recursive combiner null-value term (next a) next b))))

(define (sum term a next b)
   (accumulate-iterative + 0 term a next b))

(define (product term a next b)
  (accumulate-recursive * 1 term a next b))

(define (fib n)
  (define (next x) (+ 1 x))
  (define (term x) x)
  (product term 1 next n))

(fib 4)
  