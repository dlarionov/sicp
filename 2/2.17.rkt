#lang racket

(define (list-ref items n)
  (define (iter state count)
    (if (= count n)
        (car state)
        (iter (cdr state) (+ 1 count))))
  (iter items 0))

(define (length items)
  (define (iter state count)
    (if (null? state)
        count
        (iter (cdr state) (+ 1 count))))
  (iter items 0))

(define (last-pair-bad items)
  (let ((n (length items)))
    (list-ref items (- n 1))))

(define (last-pair-good items)
  (define (iter state count)
    (let ((next (cdr state)))
      (if (null? next)
          state
          (iter next (+ 1 count)))))
  (iter items 0))

(define x (list 1 2 3 4 5 6 7 8 9))
(list-ref x 3)
(length x)
(last-pair-bad x)
(last-pair-good x)
