#lang racket

(define (prime? n)
  (define (next x)
    (if (= x 2)
        3
        (+ x 2)))
  (define (try x)
    (if (> (* x x) n)
        n
        (if (= (remainder n x) 0)
            x
            (try (next x)))))
  (= (try 2) n))

(define (test n)
  (define (runtime) 
    (current-milliseconds))
  (define (start time)
    (define (report-true x)
      (display x)
      (display " : ")
      (display (- (runtime) time))
      (newline)
      true)
    (if (prime? n)
        (report-true n)
        false))
  (start (runtime)))

(define (print-primes from)
  (define (iter v cnt)
    (if (> cnt 0)        
        (if (test v)
            (iter (+ v 1) (- cnt 1))
            (iter (+ v 1) cnt))
        from))
    (iter from 3))

(print-primes 10000000000000000)
(print-primes 1000000000000000000)
