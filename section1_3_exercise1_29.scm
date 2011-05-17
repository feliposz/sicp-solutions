; Definition of a general method for summing

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

; A simple integration procedure for a given function
; integral of function f, given the interval a-b is calculated numerically by:
; integral(f,a,b,dx) = [ f(a+dx/2) + f(a+dx+dx/2) + f(a+2*dx+dx/2) + ... ] * dx

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; SOME TEST CASES

(define (cube x) (* x x x))

(integral cube 0 1 0.01)

; Integral by the Simpson's Rule
; This is a more accurate method of numerical integration than the method above
; h/3 * [ y0 + 4*y1 + 2*y2 + 4*y3 + 2*y4 + ... 2*yn-2 + 4*yn-1 + yn ]
; where h  = (b - a) / n
;       yk = f(a + kh)

(define (integral-simpsons f a b n)
  (let ((h (/ (- b a) n)))
    (define (y k)
      (f (+ a (* k h))))
    (define (term-s k)
      (cond ((or (= k 0) (= k n)) (y k))
	    ((odd? k) (* 4.0 (y k)))
	    (else (* 2.0 (y k)))))
    (define (next-s k)
      (+ k 1))
    (* (/ h 3)
       (sum term-s 0.0 next-s n))))

(integral-simpsons cube 0 1 100)


