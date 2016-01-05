#lang racket

(define (f a b)
  ((if (> b 0) + -) a b))
