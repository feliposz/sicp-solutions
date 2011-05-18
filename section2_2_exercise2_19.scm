; A modified version of count-change using lists as parameters for the
; denomination of coin-values.

(define (count-change amount coin-values)
  (cc amount coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else (+ (cc amount
		     (except-first-denomination coin-values))
		 (cc (- amount (first-denomination coin-values))
		     coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(count-change 11 us-coins) ;=> 4
(count-change 100 us-coins) ;=> 292

(count-change 11 uk-coins) ;=> 62
(count-change 20 uk-coins) ;=> 293
(count-change 30 uk-coins) ;=> 1022
(count-change 40 uk-coins) ;=> 2728
(count-change 50 uk-coins) ;=> 6149 (somewhat slow)
;(count-change 100 uk-coins) ;=> (takes a loooong time)

; Test for other orders in the list of coins

(count-change 11 (reverse us-coins)) ;=> 4
(count-change 100 (reverse us-coins)) ;=> 292

(count-change 11 (reverse uk-coins)) ;=> should be 62
(count-change 40 (reverse uk-coins)) ;=> should be 2728

; scrambled
(count-change 11 (list 50 1 5 10 25)) ;=> 4

; The order of elements in the coin-values list doesn't affect the result of
; computation because the procedure is designed to compute combinations regardless
; of the value of the coins and it's order, since every possible combination
; is tested anyway.