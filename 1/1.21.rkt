#lang racket

(define (smallest-devisor n)
  (define (find test)
    (if (> (* test test) n)
        n
        (if (= (remainder n test) 0)
            test
            (find (+ test 1)))))
  (find 2))

(smallest-devisor 199)
(smallest-devisor 1999)
(smallest-devisor 19999)


  