#lang racket
(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (% a b)
  (* (/ b 100) a))

(define (avg a b)
  (/ (+ a b) 2))

(define (sqrt x)

  (define (improve guess)
    (avg guess (/ x guess)))

  (define (good? guess)
    (< (abs (- guess (improve guess))) (% 0.001 guess)))

  (define (iter guess)
    (if (good? guess) guess
        (iter (improve guess))))

  (if (= 0 x)
      0
      (iter 1.0)))

(define (cbrt x)

  (define (improve guess)
    (/ (+
        (/ x (square guess))
        (* 2 guess))
       3))
  
  (define (good? guess)
    (< (abs (- guess (improve guess))) (% 0.001 guess)))

  (define (iter guess)
    (if (good? guess) guess
        (iter (improve guess))))

  (if (= 0 x)
      0
      (iter 1.0)))

(sqrt 4)
(square (sqrt 4))
(square (sqrt 0.004))

(cbrt 27)
(cube (cbrt 27))
(cube (cbrt 0.00027))





