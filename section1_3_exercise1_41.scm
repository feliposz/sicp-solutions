(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ 1 x))

((double inc) 10) ;=> 12
(((double (double double)) inc) 5) ;=> 21
