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

(define (make-joint acc pass new-pass)
  (lambda(x p)
    (if (eq? new-pass p)
        (acc x pass)
        "wrong password")))

(define a1 (make-account 100 'hello))
(define a2 (make-joint a1 'hello 'world))
 
(a1 'get 'hello)
(a1 10 'hello)
(a2 'get 'world)
(a2 12 'world)
(a1 'get 'hello)