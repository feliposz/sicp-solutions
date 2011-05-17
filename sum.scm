; Conventional way to define some similar procedures of summing a sequence of numbers

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cube a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cube (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (cube x)
  (* x x x))

(sum-integers 1 10)
(sum-cube 1 10)
(* 8 (pi-sum 1 1000))

; Definition of a general method for summing

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

; Redefinition of the above procedures using the generalized sum procedure

(define (inc x) (+ x 1))

(define (sum-integers a b)
  (define (identity x) x)
  (sum identity a inc b))

(define (sum-cube a b)
  (sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

; A simple integration procedure for a given function
; integral of function f, given the interval a-b is calculated numerically by:
; integral(f,a,b,dx) = [ f(a+dx/2) + f(a+dx+dx/2) + f(a+2*dx+dx/2) + ... ] * dx

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


