#lang racket

(define (<= a b)
  (not (> a b)))

(define (square x)
  (* x x))

(define (summ-of-squares a b)
  (+ (square a) (square b)))

(define (f a b c)
  (cond ((and (<= a b) (<= a c)) (summ-of-squares b c))
        ((and (<= b a) (<= b c)) (summ-of-squares a c))
        ((and (<= c a) (<= c b)) (summ-of-squares a b))))

        


  