#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound c) (car c))
(define (upper-bound c) (cdr c))

(define (add x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (radius x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

(define a (make-interval 3 5))
(define b (make-interval 1 2))

(= (+ (radius a) (radius b)) (radius (add a b)))
(= (+ (radius a) (radius b)) (radius (sub a b)))
(= (* (radius a) (radius b)) (radius (mul a b)))