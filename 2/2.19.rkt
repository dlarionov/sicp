#lang racket

(define (cc amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coins)) 0)
        (else
         (+ (cc amount (sub coins))
            (cc (- amount (top coins)) coins)))))

(define (no-more? items)
  (null? items))

(define (sub items)
  (cdr items))

(define (top items)
  (car items))

(define ru-coins (list 100 50 10 5 2 1))
(define ru-coins-unsorted (list 1 2 10 5 50 100))

(cc 100 ru-coins)
(cc 100 ru-coins-unsorted)



