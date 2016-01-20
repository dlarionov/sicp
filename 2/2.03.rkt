#lang racket

(define (square x) (* x x))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (segment-length point-a point-b)
  (let ((x1 (x-point point-a))
        (x2 (x-point point-b))
        (y1 (y-point point-a))
        (y2 (y-point point-b)))
    (sqrt (+
           (square (- x2 x1))
           (square (- y2 y1))))))

; rectangle abstraction barriers
(define (rectangle-perimeter r)
  (let ((a (rectangle-width r))
        (b (rectangle-height r)))
    (* 2 (+ a b))))

(define (rectangle-square r)
  (let ((a (rectangle-width r))
        (b (rectangle-height r)))
    (* a b)))

; 3 points rectangle view
(define (make-rectangle point-a point-b point-c)
  (cons point-a
        (cons point-b point-c)))

(define (a-point r) (car r))
(define (b-point r) (car (cdr r)))
(define (c-point r) (cdr (cdr r)))

(define (rectangle-width r)
  (segment-length (a-point r) (b-point r)))
(define (rectangle-height r)
  (segment-length (b-point r) (c-point r)))

; 3 points test
(define r (make-rectangle
           (make-point 0 0)
           (make-point 20 0)
           (make-point 20 10)))

(rectangle-perimeter r)
(rectangle-square r)

; 2 points and height rectangle view
(define (make-rectangle point-a point-b height)
  (cons height
        (cons point-a point-b)))

(define (a-point r) (car (cdr r)))
(define (b-point r) (cdr (cdr r)))

(define (rectangle-width r)
  (segment-length (a-point r) (b-point r)))
(define (rectangle-height r) (car r))

; 2 points and height test
(define r (make-rectangle
           (make-point 0 0)
           (make-point 20 0)
           10))

(rectangle-perimeter r)
(rectangle-square r)