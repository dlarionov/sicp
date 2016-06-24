#lang racket

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op
       (car seq)
       (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (enumerate n m)
  (define (iter i r)
    (if (> i m)
        r
        (iter (+ 1 i) (append r (list i)))))
  (iter n (list)))

(enumerate 1 10)

(define (unique-pairs n)
  (flatmap
   (lambda(i)
     (map (lambda(j)
            (list i j))
          (enumerate 1 (- i 1))))
     (enumerate 1 n)))

(unique-pairs 5)
