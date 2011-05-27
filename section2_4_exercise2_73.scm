; This program requires the procedures defined in examples/ch2support.scm
(load "examples\\ch2support.scm")

; Exercise 2.73
; a) Because a number and a variable are not tagged?
; b) See below.
; c) See below.
; d) Only the interface table would need to be changed.

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp)
					   var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
 
(define (install-deriv-package)
  ;; Internal stuff
  (define (deriv-sum operands var)
    (make-sum (deriv (car operands) var)
	      (deriv (cadr operands) var)))
  (define (deriv-prod operands var)
    (make-sum
     (make-product (car operands)
		   (deriv (cadr operands) var))
     (make-product (deriv (car operands) var)
		   (cadr operands))))
  (define (deriv-expt operands var)
    (make-product
     (make-product (cadr operands)
		   (make-exponentiation (car operands)
					(- (cadr operands) 1)))
     (deriv (car operands) var)))

  ;; Interface
  (define (tag x)
    (attach-tag 'deriv x))
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-prod)
  (put 'deriv '** deriv-expt)
  'done)

(install-deriv-package)

; Auxiliary functions

(define (=number? exp num)
   (and (number? exp) (= exp num)))
 
(define (make-sum a1 a2)
   (cond ((=number? a1 0) a2)
         ((=number? a2 0) a1)
         ((and (number? a1) (number? a2)) (+ a1 a2))
         (else (list '+ a1 a2))))

(define (make-product m1 m2)
   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
         ((=number? m1 1) m2)
         ((=number? m2 1) m1)
         ((and (number? m1) (number? m2)) (* m1 m2))
         (else (list '* m1 m2))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
	((=number? e 1) b)
	((=number? b 0) 0)
	((=number? b 1) 1)
	((and (number? b) (number? e)) (expt b e))
	(else (list '** b e))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))










