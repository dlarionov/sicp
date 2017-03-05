#lang racket

(define (make-monitored f)
  (let ((counter 0))
    (lambda(x)
      (cond ((eq? x 'get) counter)
            ((eq? x 'reset) (set! counter 0))
            (else (begin (set! counter (+ counter 1)) (f x)))))))

(define sqrt-monitored (make-monitored sqrt))

(sqrt-monitored 100)
(sqrt-monitored 36)
(sqrt-monitored 'get)
(sqrt-monitored 'reset)
(sqrt-monitored 1024)
(sqrt-monitored 'get)



