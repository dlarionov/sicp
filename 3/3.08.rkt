#lang racket

(define (make-f)
  (let ((y 0))
    (lambda (x)
      (if (> y 0)
          0
          (begin (set! y (+ y 1)) x)))))

(define f1 (make-f))
(define f2 (make-f))

(+ (f1 0) (f1 1))
(+ (f2 1) (f2 0))
