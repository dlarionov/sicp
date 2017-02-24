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
    ;(print (list 'apply-generic op args))
    ;(newline)
    (if (null? result)
        (error "No method for these types" (cons op (map type-tag args)))
        result)))

(define (add x y) (if (and (number? x) (number? y)) (+ x y) (apply-generic 'add x y)))
(define (sub x y) (if (and (number? x) (number? y)) (- x y) (apply-generic 'sub x y)))
(define (mul x y) (if (and (number? x) (number? y)) (* x y) (apply-generic 'mul x y)))
(define (div x y) (if (and (number? x) (number? y)) (/ x y) (apply-generic 'div x y)))
(define (zero? x) (if (number? x) (= x 0) (apply-generic 'zero? x)))
(define (negate x) (if (number? x) (- x) (apply-generic 'negate x)))

(define (coeff term) (apply-generic 'coeff term))
(define (order term) (apply-generic 'order term))
(define (first-term terms) (apply-generic 'first-term terms))
(define (rest-terms terms) (apply-generic 'rest-terms terms))
(define (adjoin-term term terms) (apply-generic 'adjoin-term term terms))
(define (empty? terms) (apply-generic 'empty? terms))
(define (reduce terms) (apply-generic 'reduce terms))

(define (install-terms-package)
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (install-dense-package)
    (define (adjoin-term term terms)      
      (let ((len (length terms)))
        (cond ((= len (order term)) (tag (cons (coeff term) terms)))
              ((< len (order term)) (adjoin-term term (cons 0 terms)))
              ((> len (order term)) (error ("terms length more than term order"))))))

    (define (reduce terms)      
      (if (zero? (car terms))
          (reduce (cdr terms))
          terms))
    
    (define (tag x) (attach-tag 'dense-terms x))
    (put 'make 'dense-terms (lambda(x) (tag x)))
    (put 'first-term '(dense-terms) (lambda(x) (tag-term (make-term (length (cdr x)) (car x)))))
    (put 'rest-terms '(dense-terms) (lambda(x) (tag (cdr x))))
    (put 'adjoin-term '(term dense-terms) adjoin-term)
    (put 'empty? '(dense-terms) (lambda(x) (null? x)))
    (put 'negate '(dense-terms) (lambda (x) (tag (map negate x))))
    (put 'reduce '(dense-terms) (lambda(x) (tag (reduce x))))
    )
  
  (define (install-sparse-package)
    (define (adjoin-term term terms)
      (if (zero? (coeff term))
          terms
          (cons term terms)))
  
    (define (tag x) (attach-tag 'sparse-terms x))
    (put 'make 'sparse-terms (lambda(x) (tag x)))
    (put 'first-term '(sparse-terms) (lambda(x) (let ((i (car x)))(tag-term (make-term (order i) (coeff i))))))
    (put 'rest-terms '(sparse-terms) (lambda(x) (tag (cdr x))))
    (put 'adjoin-term '(term sparse-terms) (lambda(x y) (tag (adjoin-term x y))))
    (put 'empty? '(sparse-terms) (lambda(x) (null? x)))
    (put 'negate '(sparse-terms) (lambda (x) (tag (map (lambda(i) (make-term (order i) (negate (coeff i)))) x))))
    (put 'reduce '(sparse-terms) (lambda(x) (tag x)))
    )  
  
  (define (tag-term x) (attach-tag 'term x))
  (put 'make 'term (lambda(order coeff) (tag-term (make-term order coeff))))
  (put 'order '(term) order)
  (put 'coeff '(term) coeff)
  (put 'zero? '(term) (lambda(x) (zero? (coeff x))))

  (install-dense-package)
  (install-sparse-package)
  )

(define (install-polynomial-package)  
  (define (make-term order coeff) ((get 'make 'term) order coeff))
  
  (define (variable? x) (symbol? x))
  (define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))
  
  (define (make-poly variable terms) (cons variable terms))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (add-terms L1 L2)
    (cond ((empty? L1) L2)
          ((empty? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))            
             (cond ((> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2)) (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else (adjoin-term
                          (make-term (order t1) (add (coeff t1) (coeff t2)))
                          (add-terms (rest-terms L1) (rest-terms L2)))))))))
  
  (define (mul-terms L1 L2)
    (if (empty? L1)
        L1
        (add-terms 
         (mul-term-by-all-terms (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty? L)
        L
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (div-terms L1 L2)
    (if (empty? L1)
        (list L1 L1)
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (attach-tag (type-tag L1) '()) L1)
              (let ((new-coeff (div (coeff t1) (coeff t2)))
                    (new-order (- (order t1) (order t2))))                
                (let ((mul-result (mul-term-by-all-terms (make-term new-order new-coeff) L2)))
                  (let ((rest-of-result (div-terms
                                         (reduce (add-terms L1 (negate mul-result)))
                                         L2)))
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
  
  (define (zero-poly? p)
    (define (iter tail)
      (if (empty? tail)
          true
          (if (zero? (first-term tail))              
              (iter (rest-terms tail))
              false)))
    (iter (term-list p)))

  (define (negate-poly p) (make-poly (variable p) (negate (term-list p))))

  (define (tag x) (attach-tag 'polynomial x))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))  
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (let ((result (div-poly p1 p2))) (list (tag (car result)) (tag (cadr result))))))
  (put 'zero? '(polynomial) (lambda (x) (zero-poly? x)))
  (put 'negate '(polynomial) (lambda (x) (tag (negate-poly x))))
  )

(install-terms-package)
(install-polynomial-package)

(define (make-sparse-terms x) ((get 'make 'sparse-terms) x))
(define (make-dense-terms x) ((get 'make 'dense-terms) x))
(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))

(define y (make-polynomial 'y (make-dense-terms (list 1 0 1))))
(define x (make-polynomial 'x (make-dense-terms (list 1 -1))))
(define p1 (make-polynomial 'x (make-sparse-terms (list (list 2 y) (list 0 -1)))))
(define p2 (make-polynomial 'y (make-sparse-terms (list (list 1 x) (list 0 x)))))

(add p1 p2)