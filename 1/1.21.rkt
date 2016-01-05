#lang racket

(define (square x)
  (* x x))

(define (smallest-devisor n)
  (define (devisor? i)
    (= (remainder n i) 0))
  (define (iter i)
    (if (> (square i) n)
        n
        (if (devisor? i)
            i
            (iter (+ i 1)))))
  (iter 2))

(smallest-devisor 199)
(smallest-devisor 1999)
(smallest-devisor 19999)


