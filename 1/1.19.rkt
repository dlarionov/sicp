#lang racket

(define (fib n)
  (define (iter a b p q cnt)
    (cond ((= cnt 0) b)
          ((= (remainder cnt 2) 0)
           (iter
            a
            b
            (+ (* q q) (* p p)) ; p1
            (+ (* q q) (* 2 p q)) ; q1
            (/ cnt 2)))
          (else
           (iter
            (+ (* b q) (* a q) (* a p)) ; a < a + b
            (+ (* b p) (* a q)) ; b < a
            p
            q
            (- cnt 1)))))
  (iter 1 0 0 1 n)) ; p = 0, q = 1

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)