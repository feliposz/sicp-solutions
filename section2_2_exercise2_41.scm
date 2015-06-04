; Write a procedure to find all ordered triples of distinct
; positive integers i, j, and k less than or equal to a given
; integer n that sum to a given integer s.

(define (unique-trios-sum n s)
  (define (trio-sum-is? s t)
    (= s (+ (car t) (cadr t) (caddr t))))
  (filter (lambda (t) (trio-sum-is? s t))
	  (unique-trios n)))

(define (unique-trios n)
  (accumulate append
	      nil
	      (map (lambda (i)
		     (accumulate append
				 nil
				 (map (lambda (j)
					(map (lambda (k)
					       (list i j k))
					     (enumerate-interval 1 (- j 1))))
				      (enumerate-interval 1 (- i 1 )))))
		   (enumerate-interval 1 n))))

; Utility procedures

(define accumulate fold-right)

(define nil ())

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
	    (enumerate-interval (+ low 1) high))))

; TEST CASES

(unique-trios 5)
(unique-trios-sum 7 10)



     
	   







