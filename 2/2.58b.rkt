#lang racket

(define (=number? x y) (and (number? x) (= x y)))
(define (variable? x) (symbol? x))
(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))

(define (sum? x)
  (pair?
   (filter
    (lambda(i) (eq? i '+))
    x)))

(define (make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list a '+ b))))

(define (addend x)
  (define (iter result tail)
    (if (eq? '+ (car tail))
        (if (null? (cdr result))
            (car result)
            result)
        (iter (cons (car tail) result) (cdr tail))))   
  (iter '() x))
  
(define (augend x)
  (if (eq? '+ (car x))
      (if (null? (cdr (cdr x)))
          (car (cdr x))
          (cdr x))
      (augend (cdr x))))
  
(define (product? x)
  (pair?
   (filter
    (lambda(i) (eq? i '*))
    x)))       

(define (make-product a b)
  (cond ((=number? a 0) 0)
        ((=number? b 0) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b)) (* a b))
        (else (list a '* b))))

(define (multiplier x)
  (define (iter result tail)
    (if (eq? '* (car tail))
        (if (null? (cdr result))
            (car result)
            result)
        (iter (cons (car tail) result) (cdr tail))))   
  (iter '() x))
  
(define (multiplicand x)
  (if (eq? '* (car x))
      (if (null? (cdr (cdr x)))
          (car (cdr x))
          (cdr x))
      (multiplicand (cdr x))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))        
        ))

(deriv '((x * (4 * x)) + (3 * (x + (y + 2)))) 'x)
(deriv '(x * 4 * x + 3 * (x + y + 2)) 'x)

