#lang racket

(define (list-equal? a b)
 (or
  (and
   (and (pair? a) (pair? b))
   (eq? (car a) (car b))
   (list-equal? (cdr a) (cdr b)))
  (eq? a b)))

(list-equal? '(he llo world) '(he llo world))
  