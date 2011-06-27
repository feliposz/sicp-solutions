(define (integral delayed-integrand initial-value dt)
   (define int
     (cons-stream initial-value
                  (let ((integrand (force delayed-integrand)))
                    (add-streams (scale-stream dt integrand)
                                 int))))
   int)
 
(define (solve f y0 dt)
   (define y (integral (delay dy) y0 dt))
   (define dy (stream-map f y))
   y)
 

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)



; Exercise 3.77

(define (integral delayed-integrand initial-value dt)
   (cons-stream initial-value
                (if (stream-null? (force delayed-integrand))
                    the-empty-stream
                    (integral (stream-cdr (force delayed-integrand))
                              (+ (* dt (stream-car (force delayed-integrand)))
                                 initial-value)
                              dt))))



  