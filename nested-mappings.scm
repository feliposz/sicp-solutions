;(define accumulate fold-right)

(define (produce-pais n)
  (accumulate append
	      nil
	      (map (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))



(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (= n (smallest-divisor n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap (lambda (i)
			  (map (lambda (j)
				 (list i j))
			       (enumerate-interval 1 (- i 1))))
			(enumerate-interval 1 n)))))
		

; TEST CASES

(produce-pairs 4)

(prime-sum? (list 1 2))

(make-pair-sum (list 1 2))

(prime-sum-pairs 6)