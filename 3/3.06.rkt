#lang racket

(define (rand-update x) (+ x 1))

(define (make-rand x)
  (lambda(symbol)  
    (cond ((eq? symbol 'reset)
           (lambda(new-x)
             (set! x new-x)))
          ((eq? symbol 'generate)
           (begin (set! x (rand-update x)) x))
          (else (error "wrong symbol")))))

(define rand (make-rand 42))

(rand 'generate)
(rand 'generate)
((rand 'reset) 42)
(rand 'generate)
(rand 'generate)