; Definition of a general method for summing (Recursive)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

; Iterative version

(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

; TEST CASES

(define (cube x)
  (* x x x))

(sum cube 1 1+ 10)

(sum-i cube 1 1+ 10)