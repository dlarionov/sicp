#lang racket

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (product x y)  
  (define (iter a b c)
    (if (= b 1)
        (+ a c)
        (if (even? b)
            (iter (double a) (halve b) c)
            (iter a (- b 1) (+ a c)))))
  (iter x y 0))

(product 6 7)