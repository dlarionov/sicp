#lang racket

(define (union-set a b)
  (if (null? a) b
      (if (null? b) a          
          (let ((x (car a)) (y (car b)))
            (cond ((= x y)
                   (cons x (union-set (cdr a) (cdr b))))
                  ((< x y)
                   (cons x (union-set (cdr a) b)))
                  ((> x y)
                   (cons y (union-set a (cdr b)))))))))

(union-set '(2 3 4) '(1 3 4 5))