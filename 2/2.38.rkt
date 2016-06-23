#lang racket

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter
         (op result (car rest))
         (cdr rest))))
  (iter init seq))

(define (fold-right op init seq)
  (if (null? seq)
      init
      (op
       (car seq)
       (fold-right op init (cdr seq)))))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))

(fold-right list null (list 1 2 3))
(fold-left list null (list 1 2 3))

(fold-right + 4 (list 1 2 3))
(fold-left + 4 (list 1 2 3))



