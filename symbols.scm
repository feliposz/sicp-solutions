; Direct implementation 
; - hard to read
; - hard to extend safely
; - hard to change representation of expressions

(define (deriv expr var)
  (if (simpl-expr? expr)
      (if (number? expr)
	  0
	  (if (eq? expr var)
	      1
	      0))
      (if (eq? (car expr) '+)
	  (list '+
		(deriv (cadr expr) var)
		(deriv (caddr expr) var))
	  (list '+
		(list '*
		      (cadr expr)
		      (deriv (caddr expr) var))
		(list '*
		      (deriv (cadr expr) var)
		      (caddr expr))))))

(define (simpl-expr? expr)
  (not (pair? expr)))

(deriv '(+ x 3) 'x)       ;=> 1
(deriv '(+ (* x y) 5) 'x) ;=> y
(deriv '(* x x) 'x)       ;=> (+ x x)

