#lang racket

(define (first-denimination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc a n)
  (cond ((= a 0) 1)
        ((or (= n 0) (< a 0)) 0)
        ;((= n 1) 1)
        (else (+ (cc a (- n 1))
                 (cc (- a (first-denimination n)) n)))))

(define (change amount)
  (cc amount 5))

(change 11)



