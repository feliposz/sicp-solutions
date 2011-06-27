; utils

(define (divisible? x y)
  (= (remainder x y) 0))

(define (integers-starting-from n)
  (cons-stream n
	       (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define primes
   (cons-stream
    2
    (stream-filter prime? (integers-starting-from 3))))
 
(define (prime? n)
   (define (iter ps)
     (cond ((> (square (stream-car ps)) n) true)
           ((divisible? n (stream-car ps)) false)
           (else (iter (stream-cdr ps)))))
   (iter primes))
 

(define (print-n s n)
  (cond ((or (empty-stream? s) (= n 0)) (newline))
	(else (display (head s))
	      (display " ")
	      (print-n (tail s) (- n 1)))))


; Exercise 3.66

(define int-pairs (pairs integers integers))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

(define (stream-pos s value)
  (define (search s position)
    (if (equal? (stream-car s) value)
	position
	(search (stream-cdr s) (+ position 1))))
  (search s 0))

;(stream-pos int-pairs '(3 100)) ; 197
;(stream-pos int-pairs '(99 100)) ; out of memory
;(stream-pos int-pairs '(100 100)) ; out of memory



;(print-n (stream-filter (lambda (pair)
;			  (prime? (+ (car pair) (cadr pair))))
;			int-pairs)
;	 10)




; Exercise 3.67

(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
		 (stream-cdr t))
     (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s)))
    (all-pairs (stream-cdr s) (stream-cdr t)))))

;(print-n (all-pairs integers integers) 20)



; Exercise 3.68

(define (louis-pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
	       t)
   (louis-pairs (stream-cdr s) (stream-cdr t))))

;(print-n (louis-pairs integers integers) 20) ; maximum-recursion depth

; Exercise 3.70

(define (merge-weighted s1 s2 weight)
  (if (stream-null? s1)
      s2
      (if (< (weight (stream-car s1))
	     (weight (stream-car s2)))
	  (cons-stream (stream-car s1)
		       (merge-weighted (stream-cdr s1) s2 weight))
	  (cons-stream (stream-car s2)
		       (merge-weighted s1 (stream-cdr s2) weight)))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

;(print-n (weighted-pairs integers
;			 integers
;			 (lambda (p)
;			   (+ (car p) (cadr p))))
;	 20)

(define filtered (stream-filter (lambda (x)
				  (not (or (= (remainder x 2) 0)
					   (= (remainder x 3) 0)
					   (= (remainder x 5) 0))))
				integers))

;(print-n (weighted-pairs filtered
;			 filtered
;			 (lambda (p)
;			   (+ (* 2 (car p))
;			      (* 3 (cadr p))
;			      (* 5 (+ (car p) (cadr p))))))
;	 20)



; Exercise 3.71

(define (cube x) (* x x x)))

(define (sum-cubes p) (+ (cube (car p))
			 (cube (cadr p))))

(define cube-ordered
  (stream-map sum-cubes
	      (weighted-pairs integers
			      integers
			      sum-cubes)))

(define (duos s1 s2)
  (cons-stream (list (stream-car s1) (stream-car s2))
	       (duos (stream-cdr s1) (stream-cdr s2))))

(define ramanujan-numbers
  (stream-map
   car
   (stream-filter
    (lambda (pop)
      (equal? (car pop) (cadr pop)))
    (duos cube-ordered (stream-cdr cube-ordered)))))

;(print-n ramanujan-numbers 6)
;1729 4104 13832 20683 32832 39312




; Exercise 3.72

(define (sum-square p)
  (+ (square (car p))
     (square (cadr p))))
		
; all pairs of integers ordered by the sum of their squares	 
(define square-ordered
  (weighted-pairs integers
		  integers
		  sum-square))

; same thing, with the sum included in the list
(define square-list
  (stream-map
   (lambda (p)
     (list (sum-square p) p))
   square-ordered))

(define (trios s1 s2 s3)
  (cons-stream (list (stream-car s1) (stream-car s2) (stream-car s3))
	       (trios (stream-cdr s1) (stream-cdr s2) (stream-cdr s3))))

; only the pairs that have 2 other equal square sums
(define square-filtered
  (stream-filter
   (lambda (trio)
     (= (caar trio) (caadr trio) (caaddr trio)))
   (trios square-list
	  (stream-cdr square-list)
	  (stream-cdr (stream-cdr square-list)))))

;(print-n square-filtered 20)


