#lang racket

(define (compose f g)
  (lambda(x) (f (g x))))

(define (repeat f times)
  (define (iter i g)
    (if (= i times)
        g
        (iter (+ 1 i) (compose f g))))
  (iter 1 f))

(define (average-dump f)
  (lambda(x) (/ (+ (f x) x) 2)))

(define (fixed-point f guess)
  (define (try x)
    (let [(y (f x))
          (d 0.0000001)]      
      (define (good? x)
        (> d (abs (- x y))))
      (if (good? x)
          y
          (try y))))
  (try guess))

(define (fixed-point-of-transform f t guess)
  (fixed-point (t f) guess))

(define (power a b)
  (exp (* b (log a))))

(define (integer-log k n)
  (ceiling (/ (log n) (log k))))

(define (root x n)
  (fixed-point-of-transform
   (lambda(y) (/ x (power y (- n 1))))
   (repeat average-dump (integer-log 2 n))
   1.0))

(root 9 2)
(root 27 3)
(root 81 4)
(root 6561 8)
(root 14348907 15)
(root 43046721 16)
(root 1853020188851841 32)
