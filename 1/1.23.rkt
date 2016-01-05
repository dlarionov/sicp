#lang racket

(define (square x) (* x x))

(define (prime? n)
  (define (next x)
    (if (= x 2)
        3
        (+ x 2)))
  (define (devisor? x)
    (= (remainder n x) 0))
  (define (try x)
    (if (> (square x) n)
        n
        (if (devisor? x)
            x
            (try (next x)))))
  (= (try 2) n))

(define (test n)
  (define (runtime) 
    (current-milliseconds))
  (define (start time)
    (define (report-true)
      (display n)
      (display " : ")
      (display (- (runtime) time))
      (newline)
      true)
    (if (prime? n)
        (report-true)
        false))
  (start (runtime)))

(define (search-for-primes from count)
  (define (iter n i)
    (cond [(> i 0)        
           (if (test n)
               (iter (+ 1 n) (- i 1))
               (iter (+ 1 n) i))]))
  (iter from count))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
(search-for-primes 10000000 3)
(search-for-primes 100000000 3)
(search-for-primes 1000000000 3)
(search-for-primes 10000000000 3)
(search-for-primes 100000000000 3)
(search-for-primes 1000000000000 3)
(search-for-primes 10000000000000 3)
(search-for-primes 100000000000000 3)
(search-for-primes 1000000000000000 3)