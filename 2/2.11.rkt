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

(define (new-mul x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond [(and (<= 0 x1) (<= 0 x2) (<= 0 y1) (<= 0 y2))(make-interval (* x1 y1) (* x2 y2))]
          [(and (> 0 x1) (> 0 x2) (> 0 y1) (> 0 y2)) (make-interval (* x2 y2) (* x1 y1))]
          [(and (> 0 x1) (<= 0 x2) (<= 0 y1) (<= 0 y2)) (make-interval (* x1 y2) (* x2 y2))]
          [(and (> 0 y1) (<= 0 x1) (<= 0 x2) (<= 0 y2)) (make-interval (* y1 x2) (* y2 x2))]
          [(and (> 0 x1) (> 0 x2) (<= 0 y1) (<= 0 y2)) (make-interval (* x1 y2) (* x2 y1))]
          [(and (> 0 y1) (> 0 y2) (<= 0 x1) (<= 0 x2)) (make-interval (* y1 x2) (* y2 x1))]
          [(and (> 0 x1) (> 0 x2) (> 0 y1) (<= 0 y2)) (make-interval (* x1 y2) (* x1 y1))]
          [(and (> 0 y1) (> 0 y2) (> 0 x1) (<= 0 x2)) (make-interval (* y1 x2) (* y1 x1))]
          [(and (> 0 x1) (<= 0 x2) (> 0 y1) (<= 0 y2)) (mul x y)]          
          [else (error "unexpected result" x1 x2 y1 y2)]
          )))

(define a++ (make-interval 2 3))
(define a-+ (make-interval -3 2))
(define a-- (make-interval -3 -2))
(define b++ (make-interval 3 4))
(define b-+ (make-interval -4 3))
(define b-- (make-interval -4 -3))

(mul a++ b++)
(mul a-- b--)
(mul a-+ b++) 
(mul a++ b-+)
(mul a-- b++) 
(mul a++ b--)
(mul a-- b-+) 
(mul a-+ b--)
(mul a-+ b-+)

(newline)

(new-mul a++ b++)
(new-mul a-- b--)
(new-mul a-+ b++)
(new-mul a++ b-+)
(new-mul a-- b++)
(new-mul a++ b--)
(new-mul a-- b-+)
(new-mul a-+ b--)
(new-mul a-+ b-+)


