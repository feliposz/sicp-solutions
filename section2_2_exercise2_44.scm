(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(above (beside smaller smaller 0.5) painter 0.5))))
