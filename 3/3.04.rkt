#lang racket

(define (call-police) (print "police called") (newline))

(define (make-account amount pass)
  (let ((counter 0))
    (lambda(x p)
      (if (eq? pass p)
          (begin
            (set! counter 0)
            (cond ((eq? x 'get) amount)
                  ((eq? x 'reset)
                   (begin (set! amount 0) amount))
                  (else
                   (begin (set! amount (+ amount x)) amount))))
          (begin
            (if (> counter 2)
                (call-police)
                (set! counter (+ counter 1)))
            "wrong password")))))

(define a1 (make-account 100 'hello))

(a1 'get 'hello)
(a1 'get 'hell)
(a1 'get 'hell)
(a1 'get 'hell)
(a1 10 'hell)
(a1 42 'hello)