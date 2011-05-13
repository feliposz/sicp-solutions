; make-rat: <int> <int> -> Rat
(define (make-rat n d)
  (let ((g (gcd n d)))
    (list (/ n g)
	  (/ d g))))

; numer: Rat -> <int>
(define (numer r) (car r))

; numer: Rat -> <int>
(define (denom r) (cadr r))

; print-rat: Rat
(define (print-rat rat)
  (display (numer rat))
  (display "/")
  (display (denom rat)))

; *rat: Rat, Rat -> Rat
(define (+rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

; *rat: Rat, Rat -> Rat
(define (*rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


