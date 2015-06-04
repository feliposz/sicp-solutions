(define (stream-map2 proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map2
	      (cons proc (map stream-cdr argstreams))))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
		   (stream-enumerate-interval (+ low 1) high))))

(define a (stream-enumerate-interval 1 3))
(define b (stream-enumerate-interval 3 10))
(define c (stream-enumerate-interval 10 20))

(stream-for-each (lambda (x)
		   (display x) (newline))
		 (stream-map2 + a b c))

