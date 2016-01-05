#lang racket

(define (square x) (* x x)) 

(define (expmod b n m) 
  (cond ((= n 0) 1) 
        ((even? n)
         (remainder (square (expmod b (/ n 2) m)) m))
        (else 
         (remainder (* b (expmod b (- n 1) m)) m))))

(define (carmichael-test n)
  (define (good? a)
    (= (expmod a n n) a))
  (define (iter i)
    (define (report-fail) 
      (display n)
      (display " failed the Carmichael test with an argument of ")
      (display i)
      (newline))
    (define (report-ok) 
      (display n)
      (display " passed the Carmichael test")
      (newline))
    (cond [(> n i)           
           (if (good? i) 
               (iter (+ 1 i))
               (report-fail))]
          [else (report-ok)]))
  (iter 1))

(carmichael-test 561)
(carmichael-test 1105)
(carmichael-test 42)

(define (probably-prime? n) 
  (define (iter times)
    (define (test) 
      (define (try a) 
        (= (expmod a n n) a))
      (try (random (+ 1 (- n 1)))))
    (cond ((= times 0) true) 
          ((test) (iter (- times 1))) 
          (else false)))
  (iter 100))

(define (true-prime? n)
  (define (next x)
    (if (= x 2)
        3
        (+ x 2)))
  (define (try x)
    (if (> (square x) n)
        n
        (if (= (remainder n x) 0)
            x
            (try (next x)))))
  (= (try 2) n))

(define (find-carmichael-numbers limit)
  (define (iter i)
    (define (report)
      (display i)
      (newline))
    (cond [(> limit i)
           (cond [(probably-prime? i)
                  (cond [(not (true-prime? i)) (report)])])
           (iter (+ i 1))]))
  (iter 2))

(find-carmichael-numbers 100000)
