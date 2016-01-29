#lang racket

(define (for-each proc items)
  (define (iter tail)
    (cond ((not (null? tail))
           ((lambda()
              (proc (car tail))
              (iter (cdr tail)))))))
  (iter items))

(for-each
 (lambda(x) (display x) (newline))
 (list 1 2 3 4 5))
