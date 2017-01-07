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

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(list->tree '(1 3 5 7 9 11))



