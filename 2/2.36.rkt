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


(accumulate-n max 1 (list (list 1 2 3) (list 2 3 1) (list 3 1 2)))