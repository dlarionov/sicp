#lang racket

(define (square x) (* x x))

(define (square-list items)
  (define (iter a b)
    (if (null? a)
        (reverse b)
        (iter (cdr a) (cons (square (car a)) b))))
  (iter items (list)))

(square-list (list 1 2 3 4))