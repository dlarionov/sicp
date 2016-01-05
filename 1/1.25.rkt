#lang racket

(define (square x) (* x x)) 

(define (expmod b n m) 
  (cond ((= n 0) 1) 
        ((even? n)
         (remainder (square (expmod b (/ n 2) m)) m))
        (else 
         (remainder (* b (expmod b (- n 1) m)) m))))

(define (fast-exp b n)
  (cond [(= n 0) 1]
        [(even? n)
         (square (fast-exp b (/ n 2)))]
        [else 
         (* b (fast-exp b (- n 1)))]))

(define (my-expmod b n m)
  (remainder (fast-exp b n) m))

(define (prime? n) 
  (define (iter times)
    (define (test)      
      (define (try a) 
        (= (my-expmod a n n) a))
      (try (random (+ 1 (- n 1)))))
    (cond ((= times 0) true) 
          ((test) (iter (- times 1))) 
          (else false)))
  (iter 100000))

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

(search-for-primes 1000 1)
(search-for-primes 10000 1)
(search-for-primes 100000 1)