#lang racket

(list 'a 'b 'c)

(list (list 'hello))

(cdr '((x1 x2) (y1 y2) (h e llo)))

(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))

(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))