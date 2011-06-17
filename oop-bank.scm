(load "objsys.scm")

;;;
;;; Bank Account Abstractions
;;;

;;  Named-Object Class
;;
(define (create-named-object name)      ; symbol -> named-object
  (create-instance make-named-object name))

(define (make-named-object self name)
  (let ((root-part (make-root-object self)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'named-object root-part)))
        ((NAME) (lambda () name))
        (else (get-method message root-part))))))
  
;;  Account Class
;;
(define (create-account name balance)
  (create-instance make-account name balance))

(define (make-account self name balance)
  (let ((named-part (make-named-object self name)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'account named-part)))
	((BALANCE) (lambda () balance))
	((DEPOSIT) (lambda (amount)
		     (set! balance (+ balance amount))
		     (ask self 'balance)))
	((WITHDRAW) (lambda (amount)
		      (cond ((> amount balance)
			     (set! balance 0)
			     (ask self 'balance))
			    (else (set! balance (- balance amount))
				  (ask self 'balance)))))
	(else (get-method message named-part))))))

;;  Savings Class
;;
(define (create-savings name init)
  (create-instance make-savings name init))

(define (make-savings self name init)
  (let ((account (make-account self name init)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'savings account)))
	((WITHDRAW) (lambda (amount)
		      (if (> amount (- (ask account 'balance) init))
			  'not-enough-funds
			  (ask account 'withdraw amount))))
	(else (get-method message account))))))

(define (create-checking name init savings-account)
  (create-instance make-checking name init savings-account))
(define (make-checking self name init savings-account)
  (let ((account (make-account self name init)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'savings account)))
	((WITHDRAW)
         (lambda (amount)
           (let ((balance (ask account 'balance)))
             (if (> amount balance)
                 (begin
                   (ask account 'withdraw balance)
                   (ask savings-account 'withdraw (- amount balance)))
                 (ask account 'withdraw amount)))
           (ask account 'balance)))
        ((TRANSFER)
         (lambda (amount)
           (let ((balance (ask account 'balance)))
             (cond ((> amount balance)
                    (ask account 'withdraw balance)
                    (ask savings-account 'deposit balance))
                   (else
                    (ask account 'withdraw amount)
                    (ask savings-account 'deposit amount))))))
	(else (get-method message account))))))

; Example
;
;(define my-savings (create-savings 'mine 100))
;(ask my-savings 'deposit 100)
;(define my-checking (create-checking 'mine 200 my-savings))
;(ask my-checking 'withdraw 250)
;(newline)
;(ask my-savings 'balance)
;(ask my-checking 'balance)


(let ((s (create-savings 'my-s 100)))
  (let ((c (create-checking 'my-c 200 s)))
    (ask c 'deposit 10)))

(let ((s (create-savings 'my-s 100)))
  (let ((c (create-checking 'my-c 200 s)))
    (ask c 'withdraw 100)))

(let ((s (create-savings 'my-s 100)))
  (let ((c (create-checking 'my-c 200 s)))
    (ask c 'transfer 100)))

(let ((s (create-savings 'my-s 100)))
  (let ((c (create-checking 'my-c 200 s)))
    (ask c 'deposit 100)
    (ask c 'transfer 150)))

(let ((s (create-savings 'my-s 100)))
  (let ((c (create-checking 'my-c 200 s)))
    (ask c 'deposit 100)
    (ask c 'transfer 150)
    (ask s 'balance)))

(let ((s (create-savings 'my-s 100)))
  (let ((c (create-checking 'my-c 200 s)))
    (ask c 'deposit 100)
    (ask c 'transfer 150)
    (ask c 'balance)))

