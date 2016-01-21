#lang racket

; the multiplicity of the base
(define (factor b n)
  (define (iter x i)    
    (if (= (remainder x b) 0)
        (iter (/ x b) (+ 1 i))
        i))
  (iter n 0))

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car z)
  (factor 2 z))

(define (cdr z)
  (factor 3 z))

(define x (cons 3 4))
x
(car x)
(cdr x)



