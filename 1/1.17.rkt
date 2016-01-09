#lang racket

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (M a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        (else (if (even? b)
                  (double (M a (halve b)))
                  (+ a (M a (- b 1)))))))

(M 9 6)

