#lang racket

(define (P r c)
  (if (or (= c 1) (= c r))
      1
      (+
       (P (- r 1) (- c 1))
       (P (- r 1) c)
       )))

(P 6 3)
(P 7 4)
(P 6 5)
(P 6 6)

