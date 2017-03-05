#lang racket

(define (make-account amount pass)
  (lambda(x p)
    (if (eq? pass p)
        (cond ((eq? x 'get) amount)
              ((eq? x 'reset)
               (begin (set! amount 0) amount))
              (else
               (begin (set! amount (+ amount x)) amount)))         
        "wrong password")))

(define a1 (make-account 100 'hello))

(a1 'get 'hello)
(a1 'get 'hell)
(a1 10 'hello)
(a1 'reset 'hello)
(a1 42 'hello)