; NOTE: THIS VERSION SUPPORT MULTIPLE OPERANDS FOR + AND *

(define nil '())

; CHANGED FROM PREFIX OPERANDS TO NORMAL OPERANDS
; EX: (a + b + c), (x * y * z), (b ** e) istead of (+ a b c), (* x y z), (** b e)

; ======================================
; BUG
; Operator precedence is not considered!
;
; Must implement a way to do this!!!!!!!
; ======================================

; The body of the deriv procedure, implemented with wishful thinking
; ignoring data representation at first
(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum (make-product (multiplier exp)
				 (deriv (multiplicand exp) var))
		   (make-product (deriv (multiplier exp) var)
				 (multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp)
					     (- (exponent exp) 1)))
	  (deriv (base exp) var)))
	(else (error "unknown expression type - DERIV" exp))))


; Helper functions for the deriv procedure

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;Sum is a list composed of '+ and at least 2 argument
(define (sum? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '+)))

;TEST: (sum? '(a + b))

	
;Sum is a list composed of '* and at least 2 argument
(define (product? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '*)))

;TEST: (product? '(a * b))

; Make an exponentiation usign operator '**
; Simplify if possible
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
	((=number? e 1) b)
	((=number? b 0) 0)
	((=number? b 1) 1)
	((and (number? b) (number? e)) (expt b e))
	(else (list b '** e))))

(define (exponentiation? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '**)))

;NOTE: not sure if this is conceptually valid
(define (base x)
  (if (exponentiation? x)
      (car x)
      x))

(define (exponent x)
  (if (exponentiation? x)
      (caddr x)
      1))

(define (make-sum . arguments)
  (make-sum-iter arguments nil 0))

; TODO: keep order of arguments...
(define (make-sum-iter arguments variables constant-sum)
  (cond ((null? arguments)
	 (if (null? variables)
	     constant-sum
	     (if (= constant-sum 0)
		 (if (null? (cdr variables))
		     (car variables)
		     (build-operator-list '+ variables))
		 (build-operator-list '+ (cons constant-sum variables)))))
	((number? (car arguments))
	 (make-sum-iter (cdr arguments)
			variables
			(+ constant-sum (car arguments))))
	(else
	 (make-sum-iter (cdr arguments)
			(cons (car arguments)
			      variables)
			constant-sum))))



(define (build-operator-list operator operands)
  (cond ((null? operands) nil)
	((null? (cdr operands)) operands) ; Last element on the list?
	(else (cons (car operands)
		    (cons operator
			  (build-operator-list operator (cdr operands)))))))

(define (addend s)
  (if (sum? s)
      (car s)
      (error "Not a valid sum:" s)))

(define (augend s)
  (if (sum? s)
      (if (null? (cdddr s))
	  (caddr s)
	  (cddr s))		 
      (error "Not a valid sum:" s)))

(define (make-product . arguments)
  (make-product-iter arguments nil 1))

(define (make-product-iter arguments variables constant-prod)
  (cond ((null? arguments)
	 (if (null? variables)
	     constant-prod
	     (cond ((= constant-prod 0) 0)
		   ((= constant-prod 1)
		    (if (null? (cdr variables))
			(car variables)
			(build-operator-list '* variables)))
		   (else (build-operator-list '* (cons constant-prod variables)))))
	 )
	((number? (car arguments))
	 (make-product-iter (cdr arguments)
			    variables
			    (* constant-prod (car arguments))))
	(else
	 (make-product-iter (cdr arguments)
			    (cons (car arguments)
				  variables)
			    constant-prod))))

(define (multiplier p)
  (if (product? p)
      (car p)
      (error "Not a valid product:" p)))

(define (multiplicand p)
  (if (product? p)
      (if (null? (cdddr p))
	  (caddr p)
	  (cddr p))
      (error "Not a valid product:" p)))




; TESTS

(deriv '(x + 3) 'x) ;=> 1
(deriv '(x * y) 'x) ;=> y
(deriv '((x * y) * (x + 3)) 'x) ;=> (+ (* x y) (* y (+ x 3)))
(deriv '(x * y * (x + 3)) 'x) ;=> (+ (* x y) (* y (+ x 3)))

(deriv '(x ** 3) 'x)
(deriv '((x ** 3) + (x ** 2)) 'x)
(deriv '((x ** 3) * (x ** 2)) 'x)

(deriv '((x * x) * (x * x) * (x * x)) 'x)


(deriv '(x * y * x) 'x)

;(build-operator-list '+ (list 1 2 3 4))
;(make-product 'a 1 'x 4 'y 5)



