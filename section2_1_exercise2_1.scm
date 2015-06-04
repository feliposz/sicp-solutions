; Section 2.1 Exercise 2.1
; This version of make-rat builds a rational number with negative sign only on the numerator

; make-rat: <int> <int> -> Rat
(define (make-rat n d)
  (let ((g (gcd n d)))
    (make-normal-rat (/ n g) (/ d g))))

(define (make-normal-rat n d)
  (if (and (< d 0) (> n 0))
      (cons (- n) (- d))
      (cons n d)))

; numer: Rat -> <int>
(define (numer r) (car r))

; numer: Rat -> <int>
(define (denom r) (cdr r))

; print-rat: Rat
(define (print-rat rat)
  (display (numer rat))
  (display "/")
  (display (denom rat)))

; +rat: Rat, Rat -> Rat
(define (+rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

; -rat: Rat, Rat -> Rat
(define (-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

; *rat: Rat, Rat -> Rat
(define (*rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

; /rat: Rat, Rat -> Rat
(define (/rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (numer y) (denom x))))

; =rat: Rat, Rat -> boolean
(define (=rat x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Find the greates common divisor of 2 integers
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; To mantain consistency with the book
(define add-rat +rat)
(define sub-rat -rat)
(define mul-rat *rat)
(define div-rat /rat)
(define equal-rat? =rat)


