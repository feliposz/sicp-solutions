(define (make-interval lower higher)
  (cons lower higher))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

; the lowest possible value would be the lowest value of x - the highest of y
; the highest possible value would be the highest value of x - the lowest of y
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
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))


(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))


; --------------------------------------------------

(define i1 (make-interval 2 3))
(define i2 (make-interval 5 7))
(define i3 (make-interval 0.4 0.7))
(define i4 (make-interval 1.3 1.8))

(width-interval i1)
;Value: .5

(width-interval i2)
;Value: 1.

(width-interval i3)
;Value: .14999999999999997

(width-interval i4)
;Value: .25

(width-interval (add-interval i1 i2))
;Value: 1.5

(width-interval (sub-interval i1 i2))
;Value: 1.5

(width-interval (add-interval i3 i4))
;Value: .3999999999999999

(width-interval (sub-interval i3 i4))
;Value: .3999999999999999


;The width of the add and of the sum is always directly proportional
;to the widh f the intervals. It's a function of it.

; ----------------------------------------------------

(width-interval (mul-interval i1 i2))
;Value: 5.5

(width-interval (div-interval i1 i2))
;Value: .1571428571428572

(width-interval (mul-interval i3 i4))
;Value: .37

(width-interval (div-interval i3 i4))
;Value: .15811965811965806


; The case for multiplication and division is not so simple, because the product interval
; is composed by the lowest and largest product between the bounds of the intervals

