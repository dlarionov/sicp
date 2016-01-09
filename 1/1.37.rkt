#lang racket

(define (cont-fract-iterative n d k)
  (define (iter i x)
    (if (> i 0)
        (iter (- i 1) (/ (n i) (+ (d i) x)))
        x))
  (iter k 0))

(define (cont-fract-recursive n d k)
  (if (= k 1)
      (/ (n 1) (d 1))  
      (/ (n (- k 1)) (+ (d (- k 1)) (cont-fract-recursive n d (- k 1))))))
     
(define (n i) 1.0)
(define (d i) 1.0)

(display 1.6180327868852458)
(newline)
(display (/ 1 (cont-fract-iterative n d 12)))
(newline)
(display (/ 1 (cont-fract-recursive n d 11)))