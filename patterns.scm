(define deriv-rules
  '(

    ( (dd (?c c) (? v))              0                                 )
    ( (dd (?v v) (? v))              1                                 )
    ( (dd (?v u) (? v))              0                                 )

    ( (dd (+ (? x1) (? x2)) (? v))   (+ (dd (: x1) (: v))
					(dd (: x2) (: v)))             )

    ( (dd (* (? x1) (? x2)) (? v))   (+ (* (: x1) (dd (: x2) (: v)))
					(* (: x2) (dd (: x1) (: v))))  )

    ( (dd (** (? x) (?c n)) (? v))   (* (* (: n)
					   (** (: x) (: (- n 1))))
					(dd (: x) (: v)))              )
    ))

(define dsimp (simplifier deriv-rules))

; (dsimp '(dd (+ x y) x))
; => (+ 1 0)

(define algebra-rules
  '(
    ( ((? op) (?c e1) (?c e2))           (: (op e1 e2))               )
    ( ((? op) (? e1) (?c e2))            ((: op) (: e2) (: e1))       )
    ( (+ 0 (? e))                        (: e)                        )
    ( (* 1 (? e))                        (: e)                        )
    ( (* 0 (? e))                        0                            )
    ( (* (?c e1) (* (?c e2) (? e3)))     (* (: (* e1 e2)) (: e3))     )
    ( (+ (? e1) (+ (?c e2) (? e3)))      (+ (: e2) (+ (: e1) (: e3))) )
    ( (* (* (? e1) (? e2)) (? e3))       (* (: e1) (* (: e2) (: e3))) )
    ( (+ (?c e1) (+ (?c e2) (? e3)))     (+ (: (+ e1 e2)) (: e3))     )
    ))

'If match is successful, returns a dictionary
(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)
	((atom? pat)
	 (if (atom? exp)
	     (if (eq? pat exp)
		 dict
		 'failed)
	     'failed))
	((arbitrary-constant? pat)
	 (if (constant? exp)
	     (extend-dict pat exp dict)
	     'failed))
	((arbitrary-variable? pat)
	 (if (variable? exp)
	     (extend-dict pat exp dict)
	     'failed))
	((arbitrary-expression? pat)
	 (extend-dict pat exp dict))
	((atom? exp) 'failed)
	(else
	 (match (cdr pat)
		(cdr exp)
		(match (car pat)
		       (car exp)
		       dict)))))

(matcher '(+ (* (? x) (? y)) (? y))
	 '(+ (*    3     x)     x ) ) ;=> dictionary: x='3' y='x'

(matcher '(+ (* (? x) (? y)) (? y))
	 '(+ (*    3     x)     4 ) ) ;=> dictionary: 'failed

; Returns an expression
(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((atom? s) s)
	  ((skeleton-evaluation? s)
	   (evalute (eval-exp s) dict))
	  (else (cons (loop (car s))
		      (loop (cdr s))))))
  (loop skeleton))

(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply
       (eval (lookup (car form) dict)
	     user-initial-environment)
       (mapcar (lambda (v)
		 (lookup v dict))
	       (cdr form)))))

(define (simplifier the-rules)
  (define (simplify-exp exp)
    (try-rules (if (compound? exp)
		   (simplify-parts exp)
		   exp)))
  (define (simplify-parts exp)
    (if (null? exp)
	'()
	(cons (simplify-exp (car exp))
	      (simplify-parts (cdr exp)))))
  (define (try-rules exp)
    (define (scan rules)
      (if (null? rules)
	  exp
	  (let ((dict (match (pattern (car rules))
			     exp
			     (empty-dictionary))))
	    (if (eq? dict 'failed)
		(scan (cdr rules))
		(simplify-exp (instantiate (skeleton (car rules))
					   dict))))))
    (scan the-rules))
  simplify-exp)

; Alternative
;(define (simplify-exp exp)
;  (try-rules (if (compound? exp)
; 		  (map simplify-exp exp)
;		  exp)))

(define (empty-dictionary) '())

(define (extend-dictionary pat dat dict)
  (let ((name (variable-name pat)))
    (let ((v (assq name dict)))
      (cond ((null? v) (cons (list name dat) dict))
	    ((eq? (cadr v) dat) dict)
	    (else 'failed)))))

(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (null? v)
	var
	(cadr v))))




