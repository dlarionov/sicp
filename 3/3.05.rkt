#lang racket

(define (square x) (* x x))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (integral predicate x1 x2 y1 y2 trials)
  (*
   (monte-carlo
    trials
    (lambda()
      (predicate (random x1 x2) (random y1 y2))))
   (* (- x2 x1) (- y2 y1))))

(integral
 (lambda(x y)
   (not (> (+ (square (- x 5)) (square (- y 7))) (square 3))))
 2 8 4 10
 100000)

(* pi 3 3)

(integral
 (lambda(x y)
   (not (> (+ (square (- x 2)) (square (- y 2))) 1)))
 1 3 1 3
 100000)

pi

