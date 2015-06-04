; Definition of a general method for multiplying a sequence

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

; Iterative version

(define (product-i term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

; Definitions of a factorial function using the product procedure

(define (identity x) x)

(define (factorial x)
  (product identity 1 1+ x))

(define (factorial-i x)
  (product-i identity 1 1+ x))

; Method to compute an approximation of PI
; by John Wallis (XVII century English Mathematitian
; PI/4 = (2*4*4*6*6*8*8*...) / (3*3*5*5*7*7*...)


(define (pi-wallis n)
  (define (term-n i) ;generates a sequence 2 4 4 6 6 8 8 ...
    (+ 2 (* 2 (floor (/ (+ i 1) 2)))))
  (define (term-d i) ;generates a sequence 3 3 5 5 7 7 9 ...
    (+ 3 (* 2 (floor (/ i 2)))))
  (* 4.0 (/ (product-i term-n 0 1+ n)
	    (product-i term-d 0 1+ n))))

; TEST CASES

(factorial 0)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)

(factorial-i 3)
(factorial-i 10)
(factorial-i 20)

;(pi-wallis 10000)
;Value: 3.1414356249917024

