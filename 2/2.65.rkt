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

(define (tree->list tree)
  (define (copy tree result)
    (if (null? tree)
        result
        (copy (left tree)
              (cons (entry tree)
                    (copy (right tree) result)))))
  (copy tree '()))

(define (list->tree arr)
  (define (partial-tree arr n)
    (if (= n 0)
        (cons '() arr)
        (let ((left-size (quotient (- n 1) 2)))
          (let ((left-result (partial-tree arr left-size)))
            (let ((left-tree (car left-result))
                  (non-left-elts (cdr left-result))
                  (right-size (- n (+ left-size 1))))
              (let ((this-entry (car non-left-elts))
                    (right-result (partial-tree (cdr non-left-elts) right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                  (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))
  (car (partial-tree arr (length arr))))

(define (union-set a b)
  (if (null? a) b
      (if (null? b) a          
          (let ((x (car a)) (y (car b)))
            (cond ((= x y)
                   (cons x (union-set (cdr a) (cdr b))))
                  ((< x y)
                   (cons x (union-set (cdr a) b)))
                  ((> x y)
                   (cons y (union-set a (cdr b)))))))))

(define (union-tree a b)
  (let ((x (tree->list a))
        (y (tree->list b)))
    (list->tree (union-set x y))))

(define (intersec-set a b)
  (if (or (null? a) (null? b))
      '()
      (let ((x (car a)) (y (car b)))
        (cond ((= x y)
               (cons x (intersec-set (cdr a) (cdr b))))
              ((< x y)
               (intersec-set (cdr a) b))
              ((> x y)
               (intersec-set a (cdr b)))))))

(define (intersec-tree a b)
  (let ((x (tree->list a))
        (y (tree->list b)))
    (list->tree (intersec-set x y))))