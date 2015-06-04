; Computes a continuous fraction in the form
;      n1
; -----------
; d1   +   n2
;      ---------
;      d2  +   n3
;        .    -------
;         .      + nk
;          .      ----
;                  dk

(define (cont-frac n d k)
  (define (helper i)
    (if (> i k)
	0
	(/ (n i)
	   (+ (d i) (helper (+ i 1))))))
  (helper 1))

; Iterative version
; Note: start computing nk/dk first
;       then go back computing nk-1/(dk-1 + previous)
;       until n1/(d1 + everything before it)

(define (cont-frac-i n d k)
  (define (iter i result)
    (if (= i 0)
	result
	(iter (- i 1) (/ (n i)
			 (+ (d i) result)))))	   
  (iter k 0))

(define (tan-cf x k)
  (cont-frac-i (lambda (i) (if (= i 1)
			       x
			       (- (expt x 2))))
	       (lambda (i) (+ (* (- i  1) 2) 1))
	       k))

; It doesn't change much after k = 9 for .75
(tan-cf .75 9)
;Value: .9315964599440725

(tan-cf 3.14159 10)
;Value: -2.6554829755074223e-6

(tan-cf (* 3.14159 0.25) 10)
;Value: .9999986732059835




