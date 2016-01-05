#lang racket

(define (f-recursive n)
  (cond ((< n 3) n)
  (else (+
         (f-recursive (- n 1))
         (f-recursive (- n 2))
         (f-recursive (- n 3))
         ))))

(define (f-iterative n)
  (define (iter n-1 n-2 n-3 count)
    (if (= count n)
        (+ n-1 n-2 n-3)
        (iter (+ n-1 n-2 n-3) n-1 n-2 (+ count 1))))
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        ((= n 3) 3)
        (else (iter 3 2 1 4))))

(f-recursive 5)
(f-iterative 5)

(f-iterative 2)