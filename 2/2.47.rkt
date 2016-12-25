#lang racket

(define make-vector cons)
(define xcor car)
(define ycor cdr)

(define (make-frame1 n a b) (list n a b))
(define (n-vector1 frame) (car frame))
(define (a-vector1 frame) (car (cdr frame)))
(define (b-vector1 frame) (car (cdr (cdr frame))))

(define (make-frame2 n a b) (cons n (cons a b)))
(define (n-vector2 frame) (car frame))
(define (a-vector2 frame) (car (cdr frame)))
(define (b-vector2 frame) (cdr (cdr frame)))

(define n (make-vector 1 1))
(define a (make-vector 0 1))
(define b (make-vector 1 0))

(define frame1 (make-frame1 n a b))
(n-vector1 frame1)
(a-vector1 frame1)
(b-vector1 frame1)

(define frame2 (make-frame2 n a b))
(n-vector2 frame2)
(a-vector2 frame2)
(b-vector2 frame2)
