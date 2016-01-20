#lang racket

(define (add-rat x y)
  (make-rat
   (+
    (* (numer x) (denom y))
    (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat
   (-
    (* (numer x) (denom y))
    (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat
   (* (numer x) (denom y))
   (* (denom x) (numer y))))

(define (quals-rat? x y)
  (=
   (* (numer x) (denom y))
   (* (denom x) (numer y))))

(define (make-rat x y)
  (if (< y 0)
      (make-rat (- x) (- y))      
      (let ((g (gcd x y)))
        (cons (/ x g) (/ y g)))))

(define (numer r) (car r))
(define (denom r) (cdr r))

(add-rat (make-rat 1 2) (make-rat 1 3))
(mul-rat (make-rat 1 2) (make-rat 1 3))