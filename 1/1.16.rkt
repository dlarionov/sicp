#lang racket

(define (square x) (* x x))

; O(log(n)) / O(log(n))
(define (exp-recursive base exp)
  (cond [(= exp 0) 1]
        [(even? exp) (square (exp-recursive base (/ exp 2)))]
        [else (* base (exp-recursive base (- exp 1)))]))

; O(n) / O(1)
(define (exp-iterative-bad base exp)
  (define (iter b i)
    (if (= exp i)
        b
        (if (> exp (* 2 i))
            (iter (square b) (* 2 i))
            (iter (* base b) (+ 1 i)))))
  (iter base 1))

; O(log(n)) / O(1)
(define (exp-iterative-good base exp)
  (define (iter a b n)
    (if (= 1 n)
        (* a b)
        (if (even? n)
            (iter a (square b) (/ n 2))
            (iter (* a b) b (- n 1)))))
  (iter 1 base exp))

(exp-recursive 2 64)
(exp-iterative-bad 2 64)
(exp-iterative-good 2 64)