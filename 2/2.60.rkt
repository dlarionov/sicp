#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (union-set a b) (append a b)

(union-set '(1 2 2 3 4) '(3 4 3 5))

(define (intersec-set a b)
  (cond ((or (null? a) (null? b)) '())
        ((element-of-set? (car a) b)
         (cons (car a) (intersec-set (cdr a) b)))
        (else (intersec-set (cdr a) b))))

(intersec-set '(1 2 2 3 4) '(1 3 4 3 5))