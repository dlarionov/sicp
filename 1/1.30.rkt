#lang racket

(define (sum term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (+ result (term x)))))
  (iter a 0))

(define (rectangles-integral f a b dx)
  (define (next x)
    (+ x dx))
  (* dx (sum f (+ a (/ dx 2)) next b)))

(define (simphson-integral f a b n)
  (define h (/ (- b a) n))  
  (define (next x) (+ x h))
  (define (term x)
    (define k (/ (- x a) h))
    (* (cond [(= k 0) 1]
             [(= k n) 1]
             [else (if (even? k) 2 4)])
       (f x)))
  (* (/ h 3.0) (sum term a next b)))

(define (cube x) (* x x x))
(rectangles-integral cube 0 1 .001)
(simphson-integral cube 0 1 1000)

