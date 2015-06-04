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


; Computes an approximation for e-2 (e is the base for the natural logarithms)
;
; Continuous fraction where Ni = 1
;                       and Di = the sequence 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ...

(cont-frac-i (lambda (i) 1.0)
	     (lambda (i) (if (= (remainder (+ i 1) 3) 0)
			     (expt 2 (/ (+ i 1) 3))
			     1))
	     17)

; It doesn't change after k = 17 for the computer's precision

