#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs) 
    (if (null? (car seqs)) 
        null 
        (cons (accumulate op init (map car seqs)) 
              (accumulate-n op init (map cdr seqs)))))

(define (vector-*-vector v w)
  (accumulate + 0 (map * v w)))

(vector-*-vector (list 1 2 3) (list 1 2 3))

(define (matrix-*-vector m v)
  (map
   (lambda(x) (vector-*-vector x v))
   m))

(matrix-*-vector (list (list 1 2 3) (list 3 4 5)) (list 1 2 3))

(define (transponse m)
  (accumulate-n cons null m)) 

(transponse (list (list 1 2 3) (list 4 5 6)))

(define (matix-*-matix m n)
  (let ((cols (transponse n)))
    (map
     (lambda(i) (matrix-*-vector cols i))
     m )))

(matix-*-matix
 (list (list 1 2 3) (list 4 5 6) (list 7 8 9))
 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))                






