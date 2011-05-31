;======================
; THREE PLAYER VERSION
;======================

;; 
;;  The play-loop procedure takes as its  arguments three prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  three arguments: a history of the player's previous plays and 
;;  a history of the other players' previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 9 - Adapted play-loop, print-out-results and get-scores

(define (play-loop strat0 strat1 strat2)
  (define (play-loop-iter
           strat0 strat1 strat2 count history0 history1 history2 limit)
    (cond ((= count limit) (print-out-results
                            history0 history1 history2 limit))
	  (else (let ((result0 (strat0 history0 history1 history2))
		      (result1 (strat1 history1 history0 history2))
                      (result2 (strat2 history2 history0 history1)))
		  (play-loop-iter strat0 strat1 strat2 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
                                  (extend-history result2 history2)
				  limit)))))
  (play-loop-iter strat0 strat1 strat2 0
                  the-empty-history the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 history2 number-of-games)
  ; DEBUG
  (display "p1: ")
  (display (get-probability-of-c (make-history-summary history0 history1 history2)))
  (newline)
  (display "p2: ")
  (display (get-probability-of-c (make-history-summary history1 history0 history2)))
  (newline)
  (display "p3: ")
  (display (get-probability-of-c (make-history-summary history2 history0 history1)))
  (newline)
  
  (let ((scores (get-scores history0 history1 history2)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (display "Player 3 Score:  ")
    (display (* 1.0 (/ (caddr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1 history2)
  (define (get-scores-helper history0 history1 history2 score0 score1 score2)
    (cond ((empty-history? history0)
	   (list score0 score1 score2))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1)
                                       (most-recent-play history2))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
                                     (rest-of-plays history2)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1)
                                     (+ (get-player-points 2 game) score2))))))
  (get-scores-helper history0 history1 history2 0 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

(define (extract-entry key assoc-list)
  (cond ((null? assoc-list) #f)
        ((equal? key (caar assoc-list)) (car assoc-list))
        (else (extract-entry key (cdr assoc-list)))))

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

; ----------------------------------------
; TWO-PLAYER procedures

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN-ORIG my-history other-history other-history2)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

(define (EGALITARIAN my-history other-history) 
  (define (majority-loop cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) "d" "c"))
          ((string=? (most-recent-play hist) "c")
           (majority-loop (+ 1 cs) ds (rest-of-plays hist)))
          (else
           (majority-loop cs (+ 1 ds) (rest-of-plays hist)))))
  (majority-loop 0 0 other-history))


(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

(define (EYE-FOR-TWO-EYES my-history other-history)
  (if (empty-history? my-history)
      "c"
      (if (and (not (empty-history? (rest-of-plays other-history)))
               (equal? (most-recent-play other-history) "d")               
               (equal? (most-recent-play (rest-of-plays other-history))
                       "d"))
          "d"
          "c")))

(define (MAKE-EYE-FOR-N-EYES n)
  (lambda (my-history other-history)
    (define (check-previous? hist count)
      (cond ((null? hist) #t)
            ((>= count n) #f)
            ((equal? "d" (most-recent-play hist))
             (check-previous? (rest-of-plays hist) (+ count 1)))
            (else 
             (check-previous? (rest-of-plays hist) count))))
    (if (empty-history? my-history)
        "c"
        (if (check-previous? other-history 0)
            "c"
            "d"))))

(define (make-rotating-strategy strat0 strat1 freq0 freq1)
  (lambda (my-history other-history)
    (let ((round (length my-history))
          (freq (+ freq0 freq1)))
      (if (< (remainder round freq) freq0)
          (strat0 my-history other-history)
          (strat1 my-history other-history)))))

(define (make-higher-order-spastic strats)
  (lambda (my-history other-history)
    (let ((round (length my-history))
          (freq (length strats)))
      ((list-ref strats (remainder round freq)) my-history other-history))))

(define (gentle strat gentleness-factor)
  (lambda (my-history other-history)
    (if (empty-history? my-history)
        (strat my-history other-history)
        (if (equal? "d" (strat my-history other-history))
            (if (< (random 100) (* gentleness-factor 100))
                "c"
                "d")
            "c"))))


; ----------------------------------------
; THREE-PLAYER procedures

;; Problem 10

(define (NASTY-3 my-history other-history other-history2)
  "d")

(define (PATSY-3 my-history other-history other-history2)
  "c")

(define (SPASTIC-3 my-history other-history other-history2)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (SOFT-EYE-FOR-EYE my-history other-history other-history2)
  (if (empty-history? my-history)
      "c"
      (if (or (equal? (most-recent-play other-history) "c")
              (equal? (most-recent-play other-history2) "c"))
          "c"
          "d")))

(define (TOUGH-EYE-FOR-EYE my-history other-history other-history2)
  (if (empty-history? my-history)
      "c"
      (if (and (equal? (most-recent-play other-history) "c")
               (equal? (most-recent-play other-history2) "c"))
          "c"
          "d")))

;(play-loop NASTY-3 PATSY-3 SPASTIC-3)
;(play-loop SOFT-EYE-FOR-EYE SPASTIC-3 SPASTIC-3)
;(play-loop TOUGH-EYE-FOR-EYE SPASTIC-3 SPASTIC-3)

; Problem 11

(define (make-combined-strategies strat0 strat1 combine)
  (lambda (my-history other-history other-history2)
    (let ((r1 (strat0 my-history other-history))
          (r2 (strat1 my-history other-history2)))
      (combine r1 r2))))

(define TOUGH-COMB
  (make-combined-strategies
   EYE-FOR-EYE EYE-FOR-EYE
   (lambda (r1 r2) (if (or (string=? r1 "d") (string=? r2 "d")) "d" "c"))))

(define FUZZY
  (make-combined-strategies
   EYE-FOR-EYE EGALITARIAN
   (lambda (r1 r2) (if (= (random 2) 0) r1 r2))))

;(play-loop TOUGH-COMB SPASTIC-3 FUZZY)
;(play-loop SOFT-EYE-FOR-EYE
;           NASTY-3 (make-combined-strategies (gentle NASTY 0.1)
;                                             (MAKE-EYE-FOR-N-EYES 4)
;                                             (lambda (r1 r2)
;                                               (if (= (random 2) 0) r1 r2))))

; Problem 12

; TODO complete this!

(define (make-history-summary hist-0 hist-1 hist-2)
  (define (summary-helper ccc cdc ddc ccd cdd ddd curr0 prev1 prev2)
    ;(display ccc)(display " ")(display cdc)(display " ")(display ddc)(display " ")
    ;(display ccd)(display " ")(display cdd)(display " ")(display ddd)(newline)
    (if (null? prev1) ; No more rounds to process, return result
        (list (list ccc ccd (+ ccc ccd))
              (list cdc cdd (+ cdc cdd))
              (list ddc ddd (+ ddc ddd)))
        (let ((c0 (most-recent-play curr0))  ; Current round of player 0
              (p1 (most-recent-play prev1))  ; Previous round of player 1
              (p2 (most-recent-play prev2))) ; Previous round of player 2
          (let ((curr-c (string=? c0 "c"))
                (curr-d (string=? c0 "d"))
                (prev-cc (and (string=? p1 "c") (string=? p2 "c")))
                (prev-dd (and (string=? p1 "d") (string=? p2 "d")))
                (prev-cd (or (and (string=? p1 "c") (string=? p2 "d"))
                             (and (string=? p1 "d") (string=? p2 "c")))))
            ; Iterate through history of plays, incrementing counters
            ; according to current play and previous plays of other players
            (summary-helper
             (+ ccc (if (and prev-cc curr-c) 1 0))
             (+ cdc (if (and prev-cd curr-c) 1 0))
             (+ ddc (if (and prev-dd curr-c) 1 0))
             (+ ccd (if (and prev-cc curr-d) 1 0))
             (+ cdd (if (and prev-cd curr-d) 1 0))
             (+ ddd (if (and prev-dd curr-d) 1 0))
             (rest-of-plays curr0)
             (rest-of-plays prev1)
             (rest-of-plays prev2))))))
  (summary-helper 0 0 0 0 0 0 hist-0 (rest-of-plays hist-1) (rest-of-plays hist-2)))

(define (cooperate-cooperate hs) (car hs))
(define (cooperate-defect hs) (cadr hs))
(define (defect-defect hs) (caddr hs))

(define (player-cooperate trio) (car trio))
(define (player-defect trio) (cadr trio))
(define (player-total trio) (caddr trio))

;(define summary (make-history-summary
;                 (list "c" "c" "d" "d" "c" "d" "c" "c") ;hist-0
;                 (list "c" "c" "c" "d" "d" "c" "d" "c") ;hist-1
;                 (list "c" "c" "d" "d" "d" "c" "c" "c") ;hist-2
;                 ))
;
;summary ;Value: ((3 0 3) (1 1 2) (0 2 2))

; Problem 14

(define (get-probability-of-c summary)
  (let ((cc (cooperate-cooperate summary))
        (cd (cooperate-defect summary))
        (dd (defect-defect summary)))
    (list (if (= (player-total cc) 0)
              '()
              (/ (player-cooperate cc) (player-total cc)))
          (if (= (player-total cd) 0)
              '()
              (/ (player-cooperate cd) (player-total cd)))
          (if (= (player-total dd) 0)
              '()
              (/ (player-cooperate dd) (player-total dd))))))

(define summary (make-history-summary
                 (list "c" "c" "c" "c")
                 (list "d" "d" "d" "c")
                 (list "d" "d" "c" "c")))

;(get-probability-of-c summary) ;Value: (1 1 1)

(define new-summary (make-history-summary
                     (list "c" "c" "c" "d" "c")
                     (list "d" "c" "d" "d" "c")
                     (list "d" "c" "c" "c" "c")))

;(get-probability-of-c new-summary) ;Value: (0.5 1 ())

;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
(define (test-entry expected-values actual-values) 
   (cond ((null? expected-values) (null? actual-values)) 
         ((null? actual-values) #f) 
         ((or (not (car expected-values)) 
              (not (car actual-values)) 
              (= (car expected-values) (car actual-values))) 
          (test-entry (cdr expected-values) (cdr actual-values))) 
         (else #f))) 

(define (is-he-a-fool? hist0 hist1 hist2) 
   (test-entry (list 1 1 1) 
               (get-probability-of-c 
                (make-history-summary hist0 hist1 hist2))))

(define (could-he-be-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (map (lambda (elt) 
                      (cond ((null? elt) 1)
                            ((= elt 1) 1)  
                            (else 0)))
                   (get-probability-of-c (make-history-summary hist0 
                                                               hist1
                                                               hist2)))))


; BUG: Actually, it's more complicated than this...

(define (is-soft-eye? hist0 hist1 hist2) 
   (test-entry (list 1 1 0) 
               (get-probability-of-c 
                (make-history-summary hist0 hist1 hist2))))

(define (is-tough-eye? hist0 hist1 hist2) 
   (test-entry (list 1 0 0) 
               (get-probability-of-c
                (make-history-summary hist0 hist1 hist2))))

;(play-loop NASTY-3 NASTY-3 NASTY-3)
;(play-loop PATSY-3 PATSY-3 PATSY-3)
;(play-loop NASTY-3 PATSY-3 SPASTIC-3)

;(play-loop NASTY-3 NASTY-3 SOFT-EYE-FOR-EYE)
;(play-loop PATSY-3 PATSY-3 SOFT-EYE-FOR-EYE)
;(play-loop NASTY-3 PATSY-3 SOFT-EYE-FOR-EYE)