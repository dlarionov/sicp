#lang racket

(define *op-table* (make-hash))
(define (put op type proc) (hash-set! *op-table* (list op type) proc))
(define (get op type) (hash-ref *op-table* (list op type) '()))

(define *coercion-table* (make-hash))
(define (put-coerciont type1 type2 proc) (hash-set! *coercion-table* (list type1 type2) proc))
(define (get-coercion type1 type2) (hash-ref *coercion-table* (list type1 type2) '()))

(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))

(define (same-type? type-tags)
  (define (iter T tail)
    (if (pair? tail)
        (if (eq? T (car tail))
            (iter T (cdr tail))
            false)
        true))
  (if (pair? type-tags)
      (iter (car type-tags) (cdr type-tags))
      true))

(define (distinct x) x)

(define (coercion-args T1 args)
  (define (iter head tail)
    (if (null? tail)
        (reverse head)
        (let ((T2 (type-tag (car tail))))
          (if (eq? T2 T1)
              (iter (cons (car tail) head) (cdr tail))
              (let ((t2->t1 (get-coercion T2 T1)))
                (if (null? t2->t1)
                    '()
                    (iter (cons (t2->t1 (car tail)) head) (cdr tail)))))
          )))    
  (iter '() args))

(define (apply-generic op . args)  
  (define (try-coercion types)
    (define (iter tail)
      (if (null? tail)
          (error "No method for these types")
          (let ((TArgs (coercion-args (car tail) args)))
            (let ((TProc (get op (map type-tag TArgs))))
              (if TProc
                  (apply TProc (map contents TArgs))
                  (iter (cdr tail)))))))
    (iter types))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (same-type? type-tags)
              (error "No method for these types")
              (try-coercion (distinct type-tags)))))))         
        
          
;(lambda(i) (apply apply-generic (cons op i)))

(put-coerciont 't2 't1 (lambda(x) (cons 't1 (cdr x))))
(put-coerciont 't3 't1 (lambda(x) (cons 't1 (cdr x))))
(put-coerciont 't1 't2 (lambda(x) (cons 't2 (cdr x))))
(put-coerciont 't3 't2 (lambda(x) (cons 't2 (cdr x))))
(put-coerciont 't4 't2 (lambda(x) (cons 't2 (cdr x))))

;(make-set (list (cons 't1 1) (cons 't2 2) (cons 't1 3) (cons 't4 42)))