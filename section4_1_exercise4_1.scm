; Original. Evaluates arguments in the same order as underlying interpreter
(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
	(else (cons (meval (first-operand exps) env)
		    (list-of-values (rest-operands exps) env)))))

; Left-to-right
(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
	(else 
	 (let ((left-val (meval (first-operand exps) env)))
	   (cons left-val
		 (list-of-values (rest-operands exps) env))))))

; Right-to-left
(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
	(else
	 (let ((right-val (list-of-values (rest-operands exps) env)))
	   (cons (meval (first-operand exps) env)
		 right-val)))))
		   

