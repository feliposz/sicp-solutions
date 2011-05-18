; accumulate is also known as fold-right
; fold-right is implemented by scheme
(define (my-fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (my-fold-right op initial (cdr sequence)))))

; fold-left is implemented by scheme
(define (my-fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(my-fold-right / 1 (list 1 2 3))      ;=> 3/2
(my-fold-left / 1 (list 1 2 3))       ;=> 1/6
(my-fold-right list nil (list 1 2 3)) ;=> (1 (2 (3 ())))
(my-fold-left list nil (list 1 2 3))  ;=> (((() 1) 2) 3)

; For fold-right and fold-left to return the same value, the
; result of (op x y) should be equal to the result of (op y x).






