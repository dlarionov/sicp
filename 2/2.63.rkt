#lang racket

(define (make-tree entry left right) (list entry left right))
(define (entry tree)
  (if (pair? tree)
      (car tree)
      tree))
(define (left tree)
  (if (and (pair? tree) (pair? (cdr tree)))
      (cadr tree)
      '()))
(define (right tree)
  (if (and (pair? tree) (pair? (cdr tree)) (pair? (cddr tree)))
      (caddr tree)
      '()))

(define (tree->list1 tree)
  (if (null? tree)
      '()
      (append (tree->list1 (left tree))
              (cons (entry tree)
                    (tree->list1 (right tree))))))

(define (tree->list2 tree)
  (define (copy tree result)
    (if (null? tree)
        result
        (copy (left tree)
              (cons (entry tree)
                    (copy (right tree) result)))))
  (copy tree '()))

(define t1 '(7 (3 1 5) (9 () 11)))
(define t2 '(3 1 (7 5 (9 () 11))))
(define t3 '(5 (3 1) (9 7 11)))

(tree->list1 t1)
(tree->list1 t2)
(tree->list1 t3)

(tree->list2 t1)
(tree->list2 t2)
(tree->list2 t3)