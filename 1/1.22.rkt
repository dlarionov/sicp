#lang racket

(define (prime? n)
  (define (try x)
    (if (> (* x x) n)
        n
        (if (= (remainder n x) 0)
            x
            (try (+ x 1)))))
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
    (iter from 5))

(print-primes 100000000)
(print-primes 10000000000)
(print-primes 1000000000000)
(print-primes 100000000000000)
(print-primes 10000000000000000)

; (test 2147483647)
; (test 359334085968622831041960188598043661065388726959079837)