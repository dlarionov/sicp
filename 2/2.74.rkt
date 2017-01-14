#lang racket

(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (set-type type obj) (cons type obj))
(define (get-type tobj) (car tobj))
(define (get-obj tobj) (cdr tobj))

(define (person? p)
  (and (pair? p) (eq? (get-type p) 'person)))

(define (install-package-dep1)
  (define (make-person name salary position) (set-type 'person (list name salary position)))
  (define (get-name p) (if (person? p) (car (get-obj p)) null))
  (define (get-salary p) (if (person? p) (cadr (get-obj p)) null))
  
  (define db
    (list
     (make-person 'Alice '42 'Manager)
     (make-person 'Ivan '10 'Programer)))

  (define (get-record name)
    (define (iter tail)
      (if (null? tail)
          null
          (if (eq? (get-name (car tail)) name)
              (car tail)
              (iter (cdr tail)))))
    (iter db))
     
  (put 'dep1 'get-record get-record)
  (put 'dep1 'get-salary  get-salary)
  )

(define (install-package-dep2)
  (define (make-person name salary address) (set-type 'person (cons name (cons salary address))))
  (define (get-name p) (if (person? p) (car (get-obj p)) null))
  (define (get-salary p) (if (person? p) (cadr (get-obj p)) null))

  (define db
    (cons
     (make-person 'Mike '31 'Saturn)
     (cons
      (make-person 'Alex '33 'Mars)
      (make-person 'Max '13 'Pluton))))

  (define (get-record name)
    (define (iter tail)
      (if (null? tail)
          null
          (let ((i (person? tail) tail (car tail)))
            (if (eq? (get-name i) name)
                i
                (iter (cdr tail))))))                
    (iter db))
  
  (put 'dep2 'get-record get-record)
  (put 'dep2 'get-salary  get-salary)
  )

(install-package-dep1)
(install-package-dep2)

(define (get-record dep name) 
  ((get dep 'get-record) name))

(get-record 'dep1 'Ivan)
(get-record 'dep1 'Luke)
(get-record 'dep2 'Max)

(define (get-salary dep name)
  (let ((r (get-record dep name)))
    (if (pair? r)
        ((get dep 'get-salary) r)
        null)))

(get-salary 'dep1 'Ivan)
(get-salary 'dep1 'Luke)
(get-salary 'dep2 'Max)

(define (find-record name deps)
  (define (iter tail)
    (if (null? tail)
        null
        (let ((r (get-record (car tail) name)))
          (if (pair? r)
              r
              (iter (cdr tail))))))
  (iter deps))
  
(find-record 'Max '(dep1 dep2))