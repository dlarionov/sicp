#lang racket

(define (square x) (* x x))

(define (expr b n)
  (cond [(= n 0) 1]
        [(even? n) (square (expr b (/ n 2)))]
        [else (* b (expr b (- n 1)))]))

(define (expi b n)
  (define (iter value degree)
    (if (= n degree)
        value
        (if (> n (* 2 degree))
            (iter (square value) (* 2 degree))
            (iter (* b value) (+ 1 degree)))))
  (iter b 1))

(= (expr 2 64) (expi 2 64))

; expi - плох. его порядок роста все еще O(n), а не O(log(n))