#lang racket

(define (reverse items)
  (define (iter source target)
    (if (null? source)
        target
        (iter (cdr source) (cons (car source) target))))
  (iter items (list)))

(reverse (list 1 2 3 4 5 6 7 8 9))