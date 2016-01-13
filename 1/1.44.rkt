#lang racket

(define (compose f g)
  (lambda(x) (f (g x))))

(define (repeat f times)
  (define (iter i g)
    (if (= i times)
        g
        (iter (+ 1 i) (compose f g))))
  (iter 1 f))

(define (smooth f)
  (let ((dx 0.0001))
    (lambda(x) (/
                (+
                 (- (f x) dx)
                 (f x)
                 (+ (f x) dx))
                3))))

;(smooth (smooth (smooth (f x)))

(define (n-smooth f n)
  ((repeat smooth n) f))


  