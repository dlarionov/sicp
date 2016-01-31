#lang racket

(define (deep-reverse items)
  (define (iter source target)
    (if (null? source)
        target
        (iter 
         (cdr source)
         (let ((i (car source)))
           (cons 
            (if (pair? i) 
                (deep-reverse i) 
                i) 
            target)))))
  (iter items (list)))

(reverse (list 1 (list 2 3) 4 5 (list 6 7 (list 8 9))))