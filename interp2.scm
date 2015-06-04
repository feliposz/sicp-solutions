

(define (define? exp) (tag-check exp 'define*))

(define (eval exp)
  (cond
   ((number? exp) exp)
   ((sum? exp) (eval-sum exp))
   ((symbol? exp) (lookup exp))
   ((define? exp) (eval-define exp))
   (else
    (error "unknown expression " exp))))

; variation on table ADT from March 2 lecture (only difference is that
; table-get returns a binding, while original version returned a value):
; make-table void -> table
; table-get table, symbol -> (binding | null)
; table-put! table, symbol, anytype -> undef
; binding-value binding -> anytype
(define environment (make-table))

(define (lookup name)
  (let ((binding (table-get environment name)))
    (if (null? binding)
	(error "unbound variable: " name)
	(binding-value binding))))

(define (eval-define exp)
  (let ((name (cadr exp))
	(defined-to-be (caddr exp)))
    (table-put! environment name (eval defined-to-be))
    'undefined))

(eval '(define* x* (plus* 4 5)))

(eval '(plus* x* 2))

; Index to procedures that have not changed:
; procedure page line
; sum? 1 4
; eval-sum 1 13


