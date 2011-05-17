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

; Computes 1/golden-ratio

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   100)

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

(cont-frac-i (lambda (i) 1.0)
	     (lambda (i) 1.0)
	     100)
