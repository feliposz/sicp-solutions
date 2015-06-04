; I don't know how to prove this mathematically... :(

; golden ratio
(define golden (/ (+ 1 (sqrt 5)) 2))

(define inv-golden (/ (- 1 (sqrt 5)) 2))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (g n)
  (/ (expt golden n) (sqrt 5)))

(define (newfib n)
  (/ (- (expt golden n) (expt inv-golden n))
     (sqrt 5)))

(fib 10)
(newfib 10)
(g 10)

; it seems that...
; fib(n) = newfib(n) =~ g(n)

