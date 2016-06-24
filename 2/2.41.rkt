#lang racket

(define (filter predicate seq)
  (cond ((null? seq) null)
        ((predicate (car seq))
         (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

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

(define (unique-triplets n)
  (flatmap
   (lambda(i)
     (let ((m (remove i (enumerate i n))))
       (flatmap 
        (lambda(j)
          (let ((p (remove i (remove j (enumerate j n)))))
            (map (lambda(k)
                   (list i j k))
                 p)))
        m)))
   (enumerate 1 n)))

(define (sum-filtered-triplets s n)
  (filter
   (lambda(triplet)
     (= (accumulate + 0 triplet) s))
   (unique-triplets n)))

(sum-filtered-triplets 13 13)