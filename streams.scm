
; Equivalent Abbreviations
;(define (cons-stream x y)
;  (cons x (delay y)))
;(define (head s)
;  (car s))
;(define (tail s)
;  (force (cdr s)))
;(define (delay p)
;  (lambda () (memo-proc y)))
;(define (force p) (p))
;(define the-empty-stream '())
;(define (empty-stream? s)
;  (null? s))

;; some kind of memoization for a function with no arguments
;(define (memo-proc proc)
;  (let ((already-run? #f)
;	(result '()))
;    (lambda ()
;      (if already-run?
;	  result
;	  (begin
;	    (set! already-run? #t)
;	    (set! result (proc)))))))


(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter-stream pred s)
  (cond
   ((empty-stream? s) the-empty-stream)
   ((pred (head s))
    (cons-stream (head s)
		 (filter-stream pred
			 (tail s))))
   (else (filter-stream pred (tail s)))))

(define (accumulate combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
		(accumulate combiner
			    init-val
			    (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree
		   the-empty-stream)
      (append-streams
       (enumerate-tree
	(left-branch tree))
       (enumerate-tree
	(right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
		   (append-streams (tail s1)
				   s2))))

(define (enum-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
		   (enum-interval (+ low 1) high))))

(define (sum-odd-squares tree)
  (accumulate +
	      0
	      (map-stream square
			  (filter-stream odd
					 (enumerate-tree tree)))))

(define (odd-fibs n)
  (accumulate cons
	      '()
	      (filter-stream odd
			     (map fib
				  (enum-interval 1 n)))))

(define (flatten-streams stream-of-streams)
  (accumulate append-streams
	      the-empty-stream
	      stream-of-streams))

(define (flatmap f s)
  (flatten-streams (map-stream f s)))

(define (prime-sum-pairs n)
  (map-stream
   (lambda (p)
     (list (car p)
	   (cadr p)
	   (+ (car p) (cadr p))))
   (filter-stream
    (lambda (p)
      (prime? (+ (car p) (cadr p))))
    (flatmap
     (lambda (i)
       (map-stream
	(lambda (j) (list i j))
	(enum-interval 1 (- i 1))))
     (enum-interval 1 n)))))

(define (prime-sum-pairs n)
  (collect
   (list i j (+ i j))
   ((i (enum-interval 1 n))
    (j (enum-interval 1 (- i 1))))
   (prime? (+ i j))))


; give all solutions for placing n queens in a checkerboard of side n
(define (queens size)
  (define (fill-cols k)
    (if (= k 0)
	(singleton empty-board)
	(collect (adjoin-position try-row
				  k
				  rest-queens)
		 ((rest-queens (fill-cols (- k 1)))
		  (try-row (enum-interval 1 size)))
		 (safe? try-row k rest-queens))))
  (fill-cols size))


; get the Nth element of a stream (starting from 0)
(define (nth-stream n s)
  (if (= n 0)
      (head s)
      (nth-stream (- n 1) (tail s))))

; print a stream
(define (print-stream s)
  (cond ((empty-stream? s) 'done)
	(else (print (head s))
	      (print-stream (tail s)))))

; print n elements of a stream
(define (print-n s n)
  (cond ((or (empty-stream? s) (= n 0)) (newline))
	(else (display (head s))
	      (display " ")
	      (print-n (tail s) (- n 1)))))

(define (divisible? x y)
  (= (remainder x y) 0))

; method for calculating all primes
(define (sieve s)
  (cons-stream (head s)
	       (sieve (filter-stream (lambda (x)
				       (not (divisible? x (head s))))
				     (tail s)))))

(define (integers-from n)
  (cons-stream n
	       (integers-from (+ n 1))))

(define primes
  (sieve (integers-from 2)))



(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
	((empty-stream? s2) s1)
	(else
	 (cons-stream
	  (+ (head s1) (head s2))
	  (add-streams (tail s1) (tail s2))))))

(define (scale-stream c s)
  (map-stream (lambda (x) (* x c)) s))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1
			      (add-streams integers ones)))

(define (integral s initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream dt s)
			      int)))
  int)

(define fibs
  (cons-stream 0
	       (cons-stream 1
			    (add-streams fibs (tail fibs)))))


; ????????????????????????????
; y' = y²  => integral(y')
(define y (integral (delay dy) 1 .001))
(define dy (map-stream square y))


(define (integral delayed-s initial-value dt)
  (define int
    (cons-stream initial-value
		 (let ((s (force delayed-s)))
		   (add-streams (scale-stream dt s)
				int))))
  int)

;Calculate square root as a stream that approximates to the value
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess)
			       (sqrt-improve guess x))
			     guesses)))
  guesses)
(print-n (sqrt-stream 2) 10)

;Calculate a stream of approximations to pi
(define (partial-sums s)
  (cons-stream (stream-car s)
	       (add-streams (stream-cdr s)
			    (partial-sums s))))
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
(print-n pi-stream 10)


(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

(print-n (euler-transform pi-stream) 10)

(define (make-tableau transform s)
  (cons-stream s
	       (make-tableau transform
			     (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
	      (make-tableau transform s)))

(print-n (accelerated-sequence euler-transform pi-stream) 10)






(define random-init 13061981)

; From K&R - The C Programming Language
(define rand-update
  (lambda (x)
    (modulo (quotient (+ (* 1103515245 x)
                         12345)
                      65536)
            32768)))

(define random-numbers
  (cons-stream random-init
	       (stream-map rand-update random-numbers)))
 
(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))
 
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
			random-numbers))
 
(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
	      (monte-carlo cesaro-stream 0 0)))

 




