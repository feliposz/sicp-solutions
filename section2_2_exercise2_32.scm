(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x)
			    (cons (car s) x))
			  rest)))))

(subsets (list 1 2 3)) ;=> (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; The procedure subsets first creates a list of the subsets for the
; original set, except for the first element. Then it creates a lists
; of subsets combining with the first element. All it does really is
; reduce the problem for a smaller size each recursive call, until the
; base case is reached (empty set). Then it recurses back combining
; the subsets with the element it left behind before.
