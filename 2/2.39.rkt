#lang racket

(define (fold-right op init seq)
  (if (null? seq)
      init
      (op
       (car seq)
       (fold-right op init (cdr seq)))))

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter
         (op result (car rest))
         (cdr rest))))
  (iter init seq))

(define (reverse-right seq)
  (fold-right (lambda(x y) (append y (list x))) null seq))

(define (reverse-left seq)
  (fold-left (lambda(x y) (cons y x)) null seq))

(reverse-right (list 1 2 3 4))
(reverse-left (list 1 2 3 4))