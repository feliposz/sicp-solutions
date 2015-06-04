(define (below2 p1 p2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up   (transform-painter p1
					 split-point
					 (make-vect 1.0 0.5)
					 (make-vect 0.0 1.0)))
	  (paint-down (transform-painter p2
					 (make-vect 0.0 0.0)
					 (make-vect 1.0 0.0)
					 split-point)))
      (lambda (rect)
	(paint-up rect)
	(paint-down rect)))))

(define (below3 p1 p2)
  (rotate270 (beside2 (rotate90 p1) (rotate90 p2))))
