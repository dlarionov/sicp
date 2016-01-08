#lang racket

(define (filtered-accumulate combiner null-value term a next b filter?)
  (define (iter x result)
    (if (> x b)
        result
        (iter 
         (next x) 
         (if (filter? x)
             (combiner result (term x))
             result))))
  (iter a null-value))

(define (square x) (* x x))

(define (prime? n)
  (define (devisor? x)
    (= (remainder n x) 0))
  (define (try x)
    (if (> (square x) n)
        n
        (if (devisor? x)
            x
            (try (+ 1 x)))))
  (= (try 2) n))

(define (sum-of-squares-of-primes a b)
  (define (next x) (+ 1 x))
  (filtered-accumulate + 0 square a next b prime?))

(sum-of-squares-of-primes 1 100)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-of-primes-for n)
  (define (next x) (+ 1 x))
  (define (self x) x)
  (define (filter? x) (= (gcd x n) 1))
  (filtered-accumulate * 1 self 1 next n filter?))

(product-of-primes-for 10)