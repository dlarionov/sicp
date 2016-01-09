#lang racket

(define (~= a b) ; approximately equals
  (< (abs (- a b)) 0.0001 ))

(define (fixed-point f guess)  
  (define (try x n)
    (let [(y (f x))]
      (if (~= x y)
          ((lambda ()
             (display "result ")
             (display y)
             (newline)
             (display "steps ")
             (display n)               
             (newline)))
          ((lambda ()
             (display "approximation ")
             (display y)
             (newline)
             (try y (+ 1 n))))
          )))
  (try guess 1))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point (lambda (x) (* .5 (+ x (/ (log 1000) (log x))))) 2.0)
