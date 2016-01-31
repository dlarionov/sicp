#lang racket

(define (fringe tree)
  (define (iter source target)
    (if (null? source)
        target
        (iter 
         (cdr source)
         (let ((i (car source)))
           (if (pair? i)
               (iter i target)
               (cons i target))))))
  (reverse (iter tree (list))))

(fringe (list 1 (list 2 3) 4 5 (list 6 7 (list 8 9))))
