;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (else (let ((result0 (strat0 history0 history1))
		      (result1 (strat1 history1 history0)))
		  (play-loop-iter strat0 strat1 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
	   (list score0 score1))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

;; note that you will need to write extract-entry

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

; Problem 3 - Order of growth

; The procedure has to check other-history twice and
; since it is recursive, it's space OoG is n.
; count-instances-of: time = O(n), space = O(n)
; EGALITARIAN-ORIG: time = O(2n), space = O(n)
(define (EGALITARIAN-ORIG my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

; This procedure checks each element of other-history only once and
; since it is iterative, it's space OoG is constant.
; EGALITARIAN: time = O(n), space = O(1)
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

; Problem 1 - Implement extract-entry
; Extract an entry from a given association-list
; where key is equal to the key in the list
; Returns #f if item not found.
; A,List<Entry<A,B>>->Entry<A,B>
(define (extract-entry key assoc-list)
  (cond ((null? assoc-list) #f)
        ((equal? key (caar assoc-list)) (car assoc-list))
        (else (extract-entry key (cdr assoc-list)))))

;; Test cases
;(extract-entry (make-play  "c" "c") *game-association-list*)
;(extract-entry (make-play  "c" "d") *game-association-list*)
;(extract-entry (make-play  "d" "c") *game-association-list*)
;(extract-entry (make-play  "d" "d") *game-association-list*)

;Problem 2 - Play matrix

;(newline) (display "NASTY") (newline)
;(play-loop NASTY NASTY)
;(play-loop NASTY PATSY)
;(play-loop NASTY SPASTIC)
;(play-loop NASTY EGALITARIAN)
;(play-loop NASTY EYE-FOR-EYE)
;(newline) (display "PATSY") (newline)
;(play-loop PATSY PATSY)
;(play-loop PATSY SPASTIC)
;(play-loop PATSY EGALITARIAN)
;(play-loop PATSY EYE-FOR-EYE)
;(newline) (display "SPASTIC") (newline)
;(play-loop SPASTIC SPASTIC)
;(play-loop SPASTIC EGALITARIAN)
;(play-loop SPASTIC EYE-FOR-EYE)
;(newline) (display "EGALITARIAN") (newline)
;(play-loop EGALITARIAN EGALITARIAN)
;(play-loop EGALITARIAN EYE-FOR-EYE)
;(newline) (display "EYE-FOR-EYE") (newline)
;(play-loop EYE-FOR-EYE EYE-FOR-EYE)

;             NASTY     PATSY   SPASTIC EGALITARIAN EYE-FOR-EYE
;NASTY        1.0/1.0   5.0/0.0 2.9/0.5 1.03/0.99   1.03/0.99
;PATSY        0.0/5.0   3.0/3.0 1.3/4.0 3.0/3.0     3.0/3.0
;SPASTIC      0.5/2.9   4.0/1.3 2.0/2.2 2.1/2.1     2.3/2.3
;EGALITARIAN  0.99/1.03 3.0/3.0 2.1/2.1 3.0/3.0     3.0/3.0
;EYE-FOR-EYE  0.99/1.03 3.0/3.0 2.3/2.3 3.0/3.0     3.0/3.0


; Problem 4 - Implement Eye-for-two-eyes

(define (EYE-FOR-TWO-EYES my-history other-history)
  (if (empty-history? my-history)
      "c"
      (if (and (not (empty-history? (rest-of-plays other-history)))
               (equal? (most-recent-play other-history) "d")               
               (equal? (most-recent-play (rest-of-plays other-history))
                       "d"))
          "d"
          "c")))

;(play-loop EYE-FOR-TWO-EYES NASTY)
;(play-loop EYE-FOR-TWO-EYES PATSY)
;(play-loop EYE-FOR-TWO-EYES SPASTIC)
;(play-loop EYE-FOR-TWO-EYES EGALITARIAN)
;(play-loop EYE-FOR-TWO-EYES EYE-FOR-EYE)
;(play-loop EYE-FOR-TWO-EYES EYE-FOR-TWO-EYES)

; Not much different from eye-for-eye

; Problem 5 - Implement Make-eye-for-n-eyes
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

;(define n-eyes 4)
;(play-loop (MAKE-EYE-FOR-N-EYES n-eyes) NASTY)
;(play-loop (MAKE-EYE-FOR-N-EYES n-eyes) PATSY)
;(play-loop (MAKE-EYE-FOR-N-EYES n-eyes) SPASTIC)
;(play-loop (MAKE-EYE-FOR-N-EYES n-eyes) EGALITARIAN)
;(play-loop (MAKE-EYE-FOR-N-EYES n-eyes) EYE-FOR-EYE)
;(play-loop (MAKE-EYE-FOR-N-EYES n-eyes) EYE-FOR-TWO-EYES)

; Problem 6 - Implement Make-rotating-strategy
(define (make-rotating-strategy strat0 strat1 freq0 freq1)
  (lambda (my-history other-history)
    (let ((round (length my-history))
          (freq (+ freq0 freq1)))
      (if (< (remainder round freq) freq0)
          (strat0 my-history other-history)
          (strat1 my-history other-history)))))

;(play-loop (make-rotating-strategy NASTY PATSY 3 2) NASTY)
;(play-loop (make-rotating-strategy PATSY SPASTIC 1 4) NASTY)
;(play-loop (make-rotating-strategy SPASTIC EGALITARIAN 4 1) NASTY)
;(play-loop (make-rotating-strategy EGALITARIAN EYE-FOR-EYE 1 1) NASTY)
;(play-loop (make-rotating-strategy EYE-FOR-EYE NASTY 2 2) NASTY)
;(play-loop (make-rotating-strategy EYE-FOR-TWO-EYES SPASTIC 3 3) NASTY)

; Problem 7 - Implement make-higher-order-spastic
(define (make-higher-order-spastic strats)
  (lambda (my-history other-history)
    (let ((round (length my-history))
          (freq (length strats)))
      ((list-ref strats (remainder round freq)) my-history other-history))))

;(play-loop (make-higher-order-spastic (list NASTY PATSY))
;           SPASTIC)
;(play-loop (make-higher-order-spastic (list PATSY EGALITARIAN))
;           SPASTIC)
;(play-loop (make-higher-order-spastic (list EYE-FOR-EYE SPASTIC PATSY))
;           SPASTIC)
;(play-loop (make-higher-order-spastic (list EYE-FOR-TWO-EYES SPASTIC))
;           SPASTIC)

; Problem 8 - Implement gentle
(define (gentle strat gentleness-factor)
  (lambda (my-history other-history)
    (if (empty-history? my-history)
        (strat my-history other-history)
        (if (equal? "d" (strat my-history other-history))
            (if (< (random 100) (* gentleness-factor 100))
                "c"
                "d")
            "c"))))

(define slightly-gentle-Nasty (gentle NASTY 0.1))
(define slightly-gentle-Eye-for-Eye (gentle EYE-FOR-EYE 0.1))

(play-loop (gentle NASTY 0.1) NASTY)
(play-loop (gentle NASTY 1) SPASTIC)
(play-loop (gentle SPASTIC 0) SPASTIC)
(play-loop (gentle EYE-FOR-EYE 0.5) NASTY)
(play-loop slightly-gentle-Nasty slightly-gentle-Eye-for-Eye)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;	    

;(define *game-association-list*
;  (list (list (list "c" "c" "c") (list 4 4 4))
;        (list (list "c" "c" "d") (list 2 2 5))
;        (list (list "c" "d" "c") (list 2 5 2))
;        (list (list "d" "c" "c") (list 5 2 2))
;        (list (list "c" "d" "d") (list 0 3 3))
;        (list (list "d" "c" "d") (list 3 0 3))
;        (list (list "d" "d" "c") (list 3 3 0))
;        (list (list "d" "d" "d") (list 1 1 1))))


;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
;(define (test-entry expected-values actual-values) 
;   (cond ((null? expected-values) (null? actual-values)) 
;         ((null? actual-values) #f) 
;         ((or (not (car expected-values)) 
;              (not (car actual-values)) 
;              (= (car expected-values) (car actual-values))) 
;          (test-entry (cdr expected-values) (cdr actual-values))) 
;         (else #f))) 
;
;(define (is-he-a-fool? hist0 hist1 hist2) 
;   (test-entry (list 1 1 1) 
;               (get-probability-of-c 
;                (make-history-summary hist0 hist1 hist2))))
;
;(define (could-he-be-a-fool? hist0 hist1 hist2)
;  (test-entry (list 1 1 1)
;              (map (lambda (elt) 
;                      (cond ((null? elt) 1)
;                            ((= elt 1) 1)  
;                            (else 0)))
;                   (get-probability-of-c (make-history-summary hist0 
;                                                               hist1
;                                                               hist2)))))

