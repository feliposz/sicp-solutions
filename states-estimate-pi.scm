; Need to implement here a good way to generate random numbers
;(define rand
;  (let ((x random-init))
;    (lambda ()
;      (set! x (rand-update x))
;      x)))

; for the sake of simplicity
(define rand
  (lambda () (random 100000)))


(define (estimate-pi n)
  (sqrt (/ 6 (monte-carlo n cesaro))))

(define (cesaro)
  (= (gcd (rand) (rand)) 1))
     
(define (monte-carlo trials experiment)
  (define (iter remaining passed)
    (cond ((= remaining 0)
           (/ passed trials 1.0))
          ((experiment)
           (iter (- remaining 1)
                 (+ passed 1)))
          (else
           (iter (- remaining 1)
                 passed))))
  (iter trials 0))

(estimate-pi 1000000)

; stateless version

(define (estimate-pi-stateless n)
  (sqrt (/ 6 (random-gcd-test n))))

; the monte-carlo experiment is no longer general

(define (random-gcd-test trials)
  (define (iter remaining passed rand0)
    (let ((rand1 (rand-update rand0)))
      (let ((rand2 (rand-update rand1)))
        (cond ((= remaining 0)
           (/ passed trials 1.0))
          ((= (gcd rand1 rand2) 1)
           (iter (- remaining 1)
                 (+ passed 1)
                 rand2))
          (else
           (iter (- remaining 1)
                 passed
                 rand2))))))
  (iter trials 0 random-seed))
      