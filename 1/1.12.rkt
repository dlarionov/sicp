#lang racket

(define (pascal-tiangle-item row col)
  (if (or (= col 1) (= col row))
      1
      (+
       (pascal-tiangle-item (- row 1) (- col 1))
       (pascal-tiangle-item (- row 1) col)
       )))

(pascal-tiangle-item 6 3)
(pascal-tiangle-item 7 4)
(pascal-tiangle-item 6 5)
(pascal-tiangle-item 6 6)

