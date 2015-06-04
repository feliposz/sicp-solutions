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

; Table for faster multiplication
;
; Legend:
; lN = lower-bound N
; uN = upper-bound N
; min = multiplication to obtain smallest endpoint
; max = multiplication to obtain largest endpoint
;
;	case	lx	ux	ly	uy	min	max
;	0	-	-	-	-	ux*uy	lx*ly
;	1	-	-	-	+	lx*uy	lx*ly
;	2	-	-	+	-	ERROR(ly>uy)
;	3	-	-	+	+	lx*uy	ux*ly
;	4	-	+	-	-	ux*ly	lx*uy
;	5	-	+	-	+	lx*uy	max(lx*ly, ux*uy)
;	6	-	+	+	-	ERROR(ly>uy)
;	7	-	+	+	+	lx*uy	ux*uy
;	8	+	-	-	-	ERROR(lx>ux)
;	9	+	-	-	+	ERROR(lx>ux)
;	10	+	-	+	-	ERROR(lx>ux and ly>ly)
;	11	+	-	+	+	ERROR(lx>ux)
;	12	+	+	-	-	ux*ly	lx*uy
;	13	+	+	-	+	ux*ly	ux*uy
;	14	+	+	+	-	ERROR (ly > uy)
;	15	+	+	+	+	lx*ly	ux*uy

(define (mul-interval x y)
  (let ((lx (lower-bound x))
	(ux (upper-bound x))
	(ly (lower-bound y))
	(uy (upper-bound y)))
    (cond ((and  (< lx 0)  (< ux 0)  (< ly 0)  (< uy 0)) ;0
	   (make-interval (* ux uy) (* lx ly)))
	  ((and  (< lx 0)  (< ux 0)  (< ly 0) (>= uy 0)) ;1
	   (make-interval (* lx uy) (* lx ly)))
	  ((and  (< lx 0)  (< ux 0) (>= ly 0)  (< uy 0)) ;2
	   (error "mul-interval: lower-bound greater than upper in y"))
	  ((and  (< lx 0)  (< ux 0) (>= ly 0) (>= uy 0)) ;3
	   (make-interval (* lx uy) (* ux ly)))
	  ((and  (< lx 0) (>= ux 0)  (< ly 0)  (< uy 0)) ;4
	   (make-interval (* ux ly) (* lx uy)))
	  ((and  (< lx 0) (>= ux 0)  (< ly 0) (>= uy 0)) ;5
	   (make-interval (* lx uy) (max (* lx ly) (* ux uy))))
	  ((and  (< lx 0) (>= ux 0) (>= ly 0)  (< uy 0)) ;6
	   (error "mul-interval: lower-bound greater than upper in y"))
	  ((and  (< lx 0) (>= ux 0) (>= ly 0) (>= uy 0)) ;7
	   (make-interval (* lx uy) (* ux uy)))
	  ((and (>= lx 0)  (< ux 0)  (< ly 0)  (< uy 0)) ;8
	   (error "mul-interval: lower-bound greater than upper in x"))
	  ((and (>= lx 0)  (< ux 0)  (< ly 0) (>= uy 0)) ;9
	   (error "mul-interval: lower-bound greater than upper in x"))
	  ((and (>= lx 0)  (< ux 0) (>= ly 0)  (< uy 0)) ;10
	   (error "mul-interval: lower-bound greater than upper in x and y"))
	  ((and (>= lx 0)  (< ux 0) (>= ly 0) (>= uy 0)) ;11
	   (error "mul-interval: lower-bound greater than upper in x"))
	  ((and (>= lx 0) (>= ux 0)  (< ly 0)  (< uy 0)) ;12
	   (make-interval (* ux ly) (* lx uy)))
	  ((and (>= lx 0) (>= ux 0)  (< ly 0) (>= uy 0)) ;13
	   (make-interval (* ux ly) (* ux uy)))
	  ((and (>= lx 0) (>= ux 0) (>= ly 0)  (< uy 0)) ;14
	   (error "mul-interval: lower-bound greater than upper in y"))
	  ((and (>= lx 0) (>= ux 0) (>= ly 0) (>= uy 0)) ;15
	   (make-interval (* lx ly) (* ux uy))))))

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


; ----- TEST CASES ---------------------------------------------

(make-interval 1 -2)
;make-interval: lower is greater than upper

(mul-interval (make-interval -2 -1) (make-interval -5 -3))
;Case 0: (3 . 10)

(mul-interval (make-interval -2 -1) (make-interval -3 5))
;Case 1: (-10 . 6)

(mul-interval (make-interval -2 -1) (make-interval 3 5))
;Case 3: (-10 . -3)

(mul-interval (make-interval -2 1) (make-interval -5 -3))
;Case 4: (-5 . 6)

(mul-interval (make-interval -1 2) (make-interval -1 2))
;Case 5a: (-2 . 4)

(mul-interval (make-interval -3 1) (make-interval -3 1))
;Case 5b: (-3 . 9)

(mul-interval (make-interval -2 1) (make-interval 3 5))
;Case 7: (-10 . 5)

(mul-interval (make-interval 1 2) (make-interval -2 -1))
;Case 12: (-4 . -1)

(mul-interval (make-interval 1 2) (make-interval -2 3))
;Case 13: (-4 . 6)

(mul-interval (make-interval 1 2) (make-interval 2 3))
;Case 15: (2 . 6)

