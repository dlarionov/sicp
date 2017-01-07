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

(define (element-of-tree? x tree)
  (cond ((null? tree) false)
        ((= (entry tree) x) true)
        (else
         (or
          (element-of-tree? x (left tree))
          (element-of-tree? x (right tree))))))

