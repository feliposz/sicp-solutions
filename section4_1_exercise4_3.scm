; Util

(define nil '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Table implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make-table void -> table
; table-get table, symbol -> (binding | null)
; table-put! table, symbol, anytype -> undef
; binding-value binding -> anytype

;;; Table implementation using alists
;;; A table is (table data), where data is a list of bindings
;;; A binding is a two-element list (symbol anytype)

(define table-tag 'table)

;;; table -> data
(define get-table-data cadr)

;;; table, data -> undef
(define set-table-data!
  (lambda (table new-data)
    (set-car! (cdr table) new-data)))

;;; void -> table
(define make-table 
  (lambda () (list table-tag '())))

;;; table, symbol -> (binding | null)
(define table-get 
  (lambda (table name)
    (assq name (get-table-data table))))

;;; table, symbol, anytype -> undef
(define table-put! 
  (lambda (table name value)
    (set-table-data! table (cons (list name value) 
				 (get-table-data table)))))

;;; binding -> anytype
(define binding-value cadr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Eval
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define meval-table (make-table))
(define (mev-put name value) (table-put! meval-table name value))
(define (mev-get name) (table-get meval-table name))

(mev-put 'quoted (lamba (exp env) (text-of-quotation exp)))
(mev-put 'let (lambda (exp env) (meval (let->combination exp) env)))
(mev-put 'set! eval-assignment)
(mev-put 'define eval-definition)
(mev-put 'if eval-if)
(mev-put 'lambda (lambda (exp env)
		   (make-procedure (lambda-parameters exp)
				   (lambda-body exp)
				   env)))
(mev-put 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(mev-put 'cond (lambda (exp env) (eval (cond->if exp) env)))

; Exercise 4.3 - Data directed version of eval
(define (meval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	(else
	 (let ((proc (mev-get (car exp))))
	   (if (or (null? proc) (false? proc))
	       (if (application? exp)
		   (mapply (meval (operator exp) env)
			   (list-of-values (operands exp) env))
		   (error "Unknown expression type -- EVAL" exp))
	       (proc exp env))))))
