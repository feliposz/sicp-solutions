(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
		   (stream-enumerate-interval (+ low 1) high))))

(define x (stream-map show (stream-enumerate-interval 0 10)))
;0
;Value: x

(stream-ref x 5)
;1
;2
;3
;4
;5
;Value: 5

(stream-ref x 7)
;6
;7
;Value: 7
