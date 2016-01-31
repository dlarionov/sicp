#lang racket

(define (make-mobile left right)
  (list left right))
(define (left-branch m)
  (car m))
(define (right-branch m)
  (car (cdr m)))

(define (make-branch length structure)
  (list length structure))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (car (cdr b)))

(define (total-weight m)
  (+ (branch-weight (left-branch m)) (branch-weight (right-branch m))))

(define (branch-weight b)
  (let ((l (branch-length b))
        (s (branch-structure b)))
    ;(display "length:")(display l)(display " structure:")(display s)(newline)
    (+ l (if (pair? s) (total-weight s) s))))

(define a (make-branch 3 2))
(define m (make-mobile a a))
(define b (make-branch 2 m))
(define n (make-mobile b b))
(define c (make-branch 1 n))
(define k (make-mobile b c))

(total-weight m)
(total-weight n)
(total-weight k)

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

(define (sum items)
  (if (null? items)
      0
      (+ (car items) (sum (cdr items)))))

(define (total-weight-good m)
  (sum (fringe m)))

(total-weight-good m)
(total-weight-good n)
(total-weight-good k)


