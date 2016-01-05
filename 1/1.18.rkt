#lang racket

(define (M a b)
  (define (double x)
    (+ x x))
  (define (iter value param)
    (if (= b param)
        value
        (if (> b (double param))
            (iter (double value) (double param))
            (iter (+ a value) (+ 1 param)))))
  (iter a 1))

(M 8 7)
