#lang racket

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (rectangles-integral f a b dx)
  (define (next x)
    (+ x dx))
  (* dx (sum f (+ a (/ dx 2)) next b)))

(define (simphson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y i)
    (f (+ a (* i h))))
  (define (iter i)
    (if (> n i)
        (+
         (* (if (even? i) 2 4) (y i))
         (iter (+ 1 i)))           
        (y n)))
  (* (/ h 3.0) (+ (y 0) (iter 1))))

(define (good-simphson-integral f a b n)
  (define h (/ (- b a) n))  
  (define (next x) (+ x h))
  (define (term x)
    (define k (/ (- x a) h))
    (* (cond [(= k 0) 1]
             [(= k n) 1]
             [else (if (even? k) 2 4)])
       (f x)))
  (* (/ h 3.0) (sum term a next b)))

(rectangles-integral cube 0 1 .001)
(simphson-integral cube 0 1 1000)
(good-simphson-integral cube 0 1 1000)

