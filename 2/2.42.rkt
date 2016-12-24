#lang racket

(define (enumerate n m)
  (define (iter i r)
    (if (> i m)
        r
        (iter (+ 1 i) (append r (list i)))))
  (iter n (list)))

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

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-position)
        (filter
         (lambda (position) (safe? k position))
         (flatmap
          (lambda (k-1-position)
            (map (lambda (row)
                   (add-queen row k k-1-position))
                 (enumerate 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-position (list))

(define (add-queen row k k-1-position) 
  (cons row k-1-position))

(define (safe? k position)
  (let ((added-queen-row (car position))
        (added-queen-col 1))
    (define (iter col tail)
      (if (null? tail)
          true        
          (let ((next-queen-row (car tail))
                (next-queen-col col))
            (if (or (= added-queen-row next-queen-row) ; -
                    ;(= added-queen-col next-queen-col) ; |
                    (= (+ added-queen-row added-queen-col) (+ next-queen-row next-queen-col)) ; \
                    (= (+ added-queen-row (- (+ 1 k) added-queen-col)) (+ next-queen-row (- (+ 1 k) next-queen-col))) ; /                 
                    )
                false
                (iter (+ 1 col) (cdr tail))))))    
    (iter 2 (cdr position))))

(queens 8)

