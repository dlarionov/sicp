#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set a b)
  (if (null? a)
      b
      (union-set (cdr a) (adjoin-set (car a) b))))

(union-set '(1 2 3 4) '(3 4 5))



      
  

  