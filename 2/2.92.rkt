#lang racket

(define *table (make-hash))
(define (put op type proc) (hash-set! *table (list op type) proc))
(define (get op type) (hash-ref *table (list op type) '()))

(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))

(define (apply-generic op . args)  
  (define (apply-generic-internal local-args)
    (let ((type-tags (map type-tag local-args)))
      (let ((proc (get op type-tags)))
        (if (null? proc)
            '()
            (apply proc (map contents local-args))))))
  (let ((result (apply-generic-internal args)))
    ;(print (list 'apply-generic op args)) (newline)    
    (if (null? result)
        (error "No method for these types" (cons op (map type-tag args)))
        result)))

(define (add x y) (if (and (number? x) (number? y)) (+ x y) (apply-generic 'add x y)))
(define (sub x y) (if (and (number? x) (number? y)) (- x y) (apply-generic 'sub x y)))
(define (mul x y) (if (and (number? x) (number? y)) (* x y) (apply-generic 'mul x y)))
(define (div x y) (if (and (number? x) (number? y)) (/ x y) (apply-generic 'div x y)))
(define (zero? x) (if (number? x) (= x 0) (apply-generic 'zero? x)))
(define (negate x) (if (number? x) (- x) (apply-generic 'negate x)))

(define (install-polynomial-package)  
  (define (variable? x) (symbol? x))
  (define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))
  
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (adjoin-term term term-list) (if (zero? (coeff term)) term-list (cons term term-list)))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (add-terms L1 L2)
    (cond ((null? L1) L2)
          ((null? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))            
             (cond ((> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2)) (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else (adjoin-term
                          (make-term (order t1) (add (coeff t1) (coeff t2)))
                          (add-terms (rest-terms L1) (rest-terms L2)))))))))
  
  (define (mul-terms L1 L2)
    (if (null? L1)
        '()
        (add-terms 
         (mul-term-by-all-terms (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (null? L)
        '()
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (div-terms L1 L2)
    (if (null? L1)
        (list '() '())
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list '() L1)
              (let ((new-coeff (div (coeff t1) (coeff t2)))
                    (new-order (- (order t1) (order t2))))                
                (let ((mul-result (mul-term-by-all-terms (make-term new-order (negate new-coeff)) L2)))
                  (let ((rest-of-result (div-terms (add-terms L1 mul-result) L2)))
                    (list
                     (adjoin-term (make-term new-order new-coeff) (car rest-of-result))
                     (cadr rest-of-result)))
                  ))))))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
        (error "variables are not the same")))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
        (error "variables are not the same")))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((quotient-and-remainder (div-terms (term-list p1) (term-list p2))))          
          (list
           (make-poly (variable p1) (car quotient-and-remainder))
           (make-poly (variable p1) (cdr quotient-and-remainder))))
        (error "variables are not the same")))

  (define (negate-poly p)
    (make-poly
     (variable p)
     (map
      (lambda(x) (make-term (order x) (negate (coeff x))))
      (term-list p))))

  (define (tag x) (attach-tag 'polynomial x))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))  
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (let ((result (div-poly p1 p2))) (list (tag (car result)) (tag (cadr result))))))
  (put 'zero? '(polynomial) (lambda (x) (null? (term-list x))))
  (put 'negate '(polynomial) (lambda (x) (tag (negate-poly x))))
  )

(install-polynomial-package)

(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))

(define p1 (make-polynomial 'x (list (list 2 1) (list 0 -1))))
(define p2 (make-polynomial 'x (list (list 1 2) (list 0 2))))

(add p1 p2)
(div p1 p2)