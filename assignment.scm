(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
            balance)
      "Insufficiente funds"))

;(withdraw 25)
;(withdraw 25)
;(withdraw 60)
;(withdraw 15)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;(new-withdraw 25)
;(new-withdraw 25)
;(new-withdraw 60)
;(new-withdraw 15)

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

;(define w1 (make-withdraw 100))
;(define w2 (make-withdraw 100))
;(w1 50)
;(w2 70)
;(w2 40)
;(w1 40)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUN"
                       m))))
  dispatch)

;(define acc (make-account 100))
;((acc 'withdraw) 50)
;((acc 'withdraw) 60)
;((acc 'deposit) 40)
;((acc 'withdraw) 60)

(define random-init 13061981)

; From K&R - The C Programming Language
(define rand-update
  (lambda (x)
    (modulo (quotient (+ (* 1103515245 x)
                         12345)
                      65536)
            32768)))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

;(rand)
;(rand)
;(rand)
;(rand)
;(rand)
;(rand)
;(rand)
;(rand)

; Using assignment

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(estimate-pi 100)

; Without assignment (stateless)
; Need to keep track of random-update, losing modularity

(define (estimate-pi2 trials)
  (sqrt (/ 6 (random-gcd-test trials cesaro-test random-init))))
  
(define (random-gcd-test trials experiment initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1) (+ trials-passed 1) x2))
              (else
               (iter (- trials-remaining 1) trials-passed x2))))))
  (iter trials 0 initial-x))

(estimate-pi2 100)