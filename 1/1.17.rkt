#lang racket

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (product a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        (else (if (even? b)
                  (double (product a (halve b)))
                  (+ a (product a (- b 1)))))))

(product 9 6)