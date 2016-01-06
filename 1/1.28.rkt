#lang racket

(define (square x) (* x x)) 

(define (expmod b n m)
  (define (trivial?)
    (and 
     (> b 1) 
     (< b (- m 1)) 
     (= (remainder (square b) m) 1)))
  (cond ((= n 0) 1)
        ((even? n)
         (if (trivial?)
             0
             (remainder (square (expmod b (/ n 2) m)) m)))
        (else 
         (remainder (* b (expmod b (- n 1) m)) m))))

(define (miller-rabin-test n)
  (define (good? x)
    (= (expmod x n n) x))
  (define (iter i)
    (define (report-fail)
      (display n)
      (display " failed the Miller Rabin test with an argument of ")
      (display i)
      (newline))
    (define (report-ok) 
      (display n)
      (display " passed the Miller Rabin test")
      (newline))
    (cond [(> n i)           
           (if (good? i) 
               (iter (+ 1 i))
               (report-fail))]
          [else (report-ok)]))
  (iter 1))

(miller-rabin-test 561)
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 6601)
(miller-rabin-test 42)
(miller-rabin-test 10037)
