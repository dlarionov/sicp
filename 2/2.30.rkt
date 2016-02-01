#lang racket

(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (square tree))
        (else (cons 
               (square-tree (car tree)) 
               (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map
   (lambda(li)
     (if (pair? li)
         (square-tree-map li)
         (square li)))     
     tree))

(square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))


