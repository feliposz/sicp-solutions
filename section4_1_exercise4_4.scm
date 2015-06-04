(define (and? exp) (tagged-list? exp 'and))
(define (eval-and exp env)
  (define (loop clauses)
    (if (null? clauses)
	#t
	(let ((val (meval (car clauses) env)))
	  (if (true? val)
	      (if (null? (cdr clauses)) ; last?
		  val
		  (loop (cdr clauses)))
	      #f))))
  (loop (cdr exp)))

(define (or? exp) (tagged-list? exp 'or))
(define (eval-or exp env)
  (define (loop clauses)
    (if (null? clauses)
	#f
	(let ((val (meval (car clauses) env)))
	  (if (true? val)
	      val
	      (if (null? (cdr clauses)) ; last?
		  #f
		  (loop (cdr clauses)))))))
  (loop (cdr exp)))

(define (eval-and2 exp env)
  (and->if exp))
(define (and->if exp)
  (define (expand clauses)
    (if (null? clauses)
	'true
	(let ((first (car clauses))
	      (rest (cdr clauses)))
	  (if (null? rest)
	      (make-if first first 'false)
	      (make-if first (expand rest) 'false)))))
  (expand (cdr exp)))

(define (eval-or2 exp env)
  (or->if exp))
(define (or->if exp)
  (define (expand clauses)
    (if (null? clauses)
	'false
	(let ((first (car clauses))
	      (rest (cdr clauses)))
	  (if (null? rest)
	      (make-if first first 'false)
	      (make-if first first (expand rest))))))
  (expand (cdr exp)))
