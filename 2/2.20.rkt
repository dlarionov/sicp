#lang racket

(define (same-parity x . z)
  (define (iter source target)
    (if (null? source)
        (reverse target)
        (let ((sign (remainder x 2))
              (i (car source))
              (tail (cdr source)))
          (if (= sign (remainder i 2))
              (iter tail (cons i target))
              (iter tail target)))))
  (cons x (iter z (list))))

(same-parity 3 3 4 5 6 7 8 9 10 3 7)