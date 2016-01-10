#lang racket

(define (cont-fract n d k)
  (define (iter i x)
    (if (> i 0)
        (iter (- i 1) (/ (n i) (+ (d i) x)))
        x))
  (iter k 0))
     
(define (n i) 1.0)
(define (d i) 
  (let ([r (remainder i 3)]
        [n (* 2 (/ (+ 1 i) 3))])
    (cond [(= r 0) 1]
          [(= r 1) 1]
          [(= r 2) n])))

(display "2.7182818284590452353602874713527")
(newline)
(display (+ 2 (cont-fract n d 20)))