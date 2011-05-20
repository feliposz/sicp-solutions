; For exercise 2.45
(define (split op1 op2)
  (define (splitter painter n)
    (if (= n 0)
	painter
	(let ((smaller (splitter painter (- n 1))))
	  (op1 painter (op2 smaller smaller 0.5) 0.5))))
  splitter)

(define right-split (split beside above))

(define up-split (split below beside))

