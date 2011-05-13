; This is absolutely crazy stuff
; Definition: http://en.wikipedia.org/wiki/Church_encoding

(define zero
  (lambda (f)
    (lambda (x) x))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

; I have no idea if this is correct :(
(define plus
  (lambda (m n)
    (lambda (f)
      (lambda (x)
	(m (f ((n f) x)))))))



