#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound c) (car c))
(define (upper-bound c) (cdr c))

(define (mul x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div x y)
  (let ((y1 (lower-bound y))
        (y2 (upper-bound y)))
    (if (> (* y1 y2) 0)
        (mul x (make-interval 
                (/ 1.0 y1) 
                (/ 1.0 y2)))
        (error "dividing an interval which contains zero" y))))

(define a (make-interval 3 5))
(define b (make-interval -2 1))

(div a b)

