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

(define (make-width-interval c w)
  (make-interval (- c w) (+ c w)))
(define (make-percent-interval c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define i (make-percent-interval 100 0.1))
(lower-bound i)
(upper-bound i)