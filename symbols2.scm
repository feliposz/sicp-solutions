; A better implementation 
; - easier to read
; - easier to extend safely
; - easier to change representation of expressions

(define (deriv expr var)
  (cond ((number? expr) 0)
	((variable? expr) (if (eq? expr var) 1 0))
	((sum-expr? expr)
	 (make-sum (deriv (a1 expr) var)
		   (deriv (a2 expr) var)))
	((product-expr? expr)
	 (make-sum (make-product (m1 expr)
				 (deriv (m2 expr) var))
		   (make-product (deriv (m1 expr) var)
				 (m2 expr))))
	(else (error "Unknown expression"))))

(define (sum-expr? expr)
  (and (pair? expr) (eq? (car expr) '+)))

(define (product-expr? expr)
  (and (pair? expr) (eq? (car expr) '*)))

(define (variable? expr)
  (and (not (pair? expr)) (symbol? expr)))

(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
	((and (number? a1) (= a1 0)) a2)
	((and (number? a2) (= a2 0)) a1)
	(else (list '+ a1 a2))))

(define a1 cadr)
(define a2 caddr)

(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2)) (* m1 m2))
	((or (and (number? m1) (= m1 0))
	     (and (number? m2) (= m2 0)))
	 0)
	((and (number? m1) (= m1 1)) m2)
	((and (number? m2) (= m2 1)) m1)
	(else (list '* m1 m2))))

(define m1 cadr)
(define m2 caddr)

(deriv '(+ x 3) 'x)       ;=> 1
(deriv '(+ (* x y) 5) 'x) ;=> y
(deriv '(* x x) 'x)       ;=> (+ x x)

(deriv '(+ (* x (* x x)) (+ (* 2 (* x x)) (+ (* 3 x) 4))) 'x)
; Equivalent original expression (after simplification):
; x^3 + 2x^2 + 3x + 4

;Result => (+ (+ (* x (+ x x)) (* x x)) (+ (* 2 (+ x x)) 3))
; Equivalent derived expression (after simplification):
; 3x^2 + 4x + 3






