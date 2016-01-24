#lang racket

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (percent i)
  (/ 
   (- (upper-bound i) (lower-bound i))
   (+ (lower-bound i) (upper-bound i))))  

(define (make-percent-interval c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

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

(define (div x y)
  (let ((y1 (lower-bound y))
        (y2 (upper-bound y)))
    (mul x (make-interval (/ 1.0 y1) (/ 1.0 y2)))))

(define one (make-interval 1 1))
(define i (make-percent-interval 100 0.1))
(define j (make-percent-interval 200 0.2))

(define (test1 i j)
  (div (mul i j) (add i j)))

(define (test2 i j)
  (div one (add (div one i) (div one j))))

(define (show i)
  (display (center i))
  (display " and ")
  (display (width i))
  (newline))

(show (test1 i j))
(show (test2 i j))

(show (div one one))
(show (div i one))
(show (div one i))
(show (div i i))