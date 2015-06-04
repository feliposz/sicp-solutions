; int, int -> Point
(define (make-point x y)
  (cons x y))

; Point -> int
(define (x-point p)
  (car p))

; Point -> int
(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Point, Point -> Segment
(define (make-segment start end)
  (cons start end))

; Segment -> Point
(define (start-segment seg)
  (car seg))

; Segment -> Point
(define (end-segment seg)
  (cdr seg))

(define (print-segment seg)
  (print-point (start-segment seg))
  (display "-")
  (print-point (end-segment seg)))

; midpoint-segment: Segment -> Point
(define (midpoint-segment seg)
  (make-point (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2)
	      (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)))

(define (make-rect corner1 corner2)
  (make-segment corner1 corner2))

(define (corner1-rect r)
  (start-segment r))

(define (corner2-rect r)
  (end-segment r))

(define (perimeter-rect r)
  (+ (* 2 (abs (- (x-point (corner1-rect r)) (x-point (corner2-rect r)))))
     (* 2 (abs (- (y-point (corner1-rect r)) (y-point (corner2-rect r)))))))

(define (area-rect r)
  (* (abs (- (x-point (corner1-rect r)) (x-point (corner2-rect r))))
     (abs (- (y-point (corner1-rect r)) (y-point (corner2-rect r))))))


; Test ---------------------------

(define rect
  (make-rect (make-point -3 -3)
	     (make-point 2 5)))
;Value: rect

(perimeter-rect rect)
;Value: 26

(area-rect rect)
;Value: 40
