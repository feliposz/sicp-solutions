(define (make-account balance password)
  (define bad-password-counter 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password dummy)
    (set! bad-password-counter (+ bad-password-counter 1))
    (if (>= bad-password-counter 7)
        (call-the-cops)
        "Incorrect password"))
  (define (call-the-cops)
    (error "Too many wrong password attempts. Calling the police!"))
  (define (dispatch givenpw m)
    (if (not (eq? givenpw password))
        incorrect-password
        (begin
          (set! bad-password-counter 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUN"
                             m))))))
        
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
