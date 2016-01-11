#lang racket

(define (fixed-point f guess)
  (define (try x)
    (let [(y (f x))
          (d 0.0000001)]      
      (define (good? x)
        (> d (abs (- x y))))
      (if (good? x)
          y
          (try y))))
  (try guess))

(define (derivative f)
  (let [(dx 0.000001)]
    (lambda (x) (/ (- (f (+ x dx)) (f x)) dx))))

(define (newton-transform f)
  (lambda (x) (- x (/ (f x) ((derivative f) x)))))

(define (newton-method f guess)
  (fixed-point (newton-transform f) guess))

(define (cube-bad a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(define (cube-good a b c)
  (lambda (x) (+ c (* x (+ b (* x (+ a x)))))))

(newton-method (cube-good 1 1 -3) 1.0)
