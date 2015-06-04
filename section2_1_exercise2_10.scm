(define (make-interval lower higher)
  (if (> lower higher)
      (error "make-interval: lower is greater than upper"))
  (cons lower higher))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (upper-bound x) (lower-bound y)))
	(p3 (* (lower-bound x) (upper-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (interval-has-zero? y)
      (error "Interval spans zero " (lower-bound y) '- (upper-bound y))) 
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

(define (interval-has-zero? x)
 (or (= (lower-bound x) 0)
     (= (upper-bound x) 0)
     (and (< (lower-bound x) 0)
	  (> (upper-bound x) 0))))


; Tests -----------------------------

(define i1 (make-interval -3 3))
(define i2 (make-interval 4 7))
(define i3 (make-interval 0 0.2))
(define i4 (make-interval -0.5 0.2))

; works as expected
(div-interval i1 i2)
(div-interval i3 i2)
(div-interval i4 i2)

; should give errors
(div-interval i2 i1)
(div-interval i1 i3)
