#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= (car set) x) true)
        ((> (car set) x) false)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 3 '(2 3 4))

(define (intersec-set a b)
  (if (or (null? a) (null? b))
      '()
      (let ((x (car a)) (y (car b)))
        (cond ((= x y)
               (cons x (intersec-set (cdr a) (cdr b))))
              ((< x y)
               (intersec-set (cdr a) b))
              ((> x y)
               (intersec-set a (cdr b)))))))

(intersec-set '(1 2 3 4) '(3 4 5 6))

(define (adjoin-set2 x set)
  (define (iter left right)
    (if (null? right)
        (append left (list x))
        (if (> (car right) x)
            (append left (cons x right))
            (iter (append left (list (car right))) (cdr right)))))
  (if (element-of-set? x set)
      set      
      (iter '() set)))

(define (adjoin-set x set)
  (define (iter left right)
    (if (null? right)
        (reverse (cons x left))
        (if (> (car right) x)
            (append (reverse left) (cons x right))
            (iter (cons (car right) left) (cdr right)))))
  (if (element-of-set? x set)
      set      
      (iter '() set)))

(adjoin-set 1 '(3 4 6 7))
(adjoin-set 3 '(3 4 6 7))
(adjoin-set 5 '(3 4 6 7))
(adjoin-set 8 '(3 4 6 7))


