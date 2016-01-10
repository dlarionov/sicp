#lang racket

(define (square x) (* x x))

(define (cont-fract n d k)
  (define (iter i x)
    (if (> i 0)
        (iter (- i 1) (/ (n i) (+ (d i) x)))
        x))
  (iter k 0))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (* -1 (square x))))
  (define (d i)
    (- (* 2.0 i) 1))
  (cont-fract n d k))

(tan-cf 3 12)
(tan 3)