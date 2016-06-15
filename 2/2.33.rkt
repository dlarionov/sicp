#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
       null 
       sequence))

(define x (list 1 3 9))
(define (square x)
  (* x x))
(map square x)

(define (append seq1 seq2)
  (accumulate cons 
              seq2
              seq1))

(define y (list 2 4 8 16 32 64 128 256))
(append x y)

(define (lenght sequence)
  (accumulate (lambda (x y) (+ 1 y)) 
              0 
              sequence))

(lenght y)



