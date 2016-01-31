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

(define (total-weight-fringe m)
  (sum (fringe m)))

(define a (make-branch 2 1))
(define m (make-mobile a a))
(define b (make-branch 3 m))
(define n (make-mobile b b))
(define c (make-branch 4 n))
(define k (make-mobile b c))

(total-weight m)
(total-weight n)
(total-weight k)
(total-weight-fringe m)
(total-weight-fringe n)
(total-weight-fringe k)

(define (balanced? m)  
  (let ((lb (left-branch m))
        (rb (right-branch m)))
    (= 
     (* (branch-length lb) (branch-weight lb)) 
     (* (branch-length rb) (branch-weight rb)))))

(define (balanced-deep? m)
  (if (not (pair? m))
      #t
      (and 
       (balanced? m)
       (balanced-deep? (branch-structure (left-branch m)))
       (balanced-deep? (branch-structure (right-branch m))))))

(balanced-deep? m)
(balanced-deep? n)
(balanced-deep? k)
