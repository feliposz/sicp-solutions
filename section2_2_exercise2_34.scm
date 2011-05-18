; Evaluate a polynomial in the form:
; AnX^n + An-1X^n-1 + ... + A1X + A0

; Using the Horner's rule, which states:
; (...(AnX + An-1)X + ... + A1)X + A0

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ (* higher-terms x) this-coeff))
	      0
	      coefficient-sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(horner-eval 2 (list 1 3 0 5 0 1)) ;=> 79

