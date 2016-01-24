#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (percent i)
  (/ 
   (- (upper-bound i) (lower-bound i))
   (+ (lower-bound i) (upper-bound i))))  

(define (make-percent-interval c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (mul x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define i (make-percent-interval 42 0.1))
(define j (make-percent-interval 56 0.2))

(percent (mul i j))
