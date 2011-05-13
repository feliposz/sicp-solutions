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

; Tests...

(define p1 (make-point 1 2))
;Value: p1

(define p2 (make-point 3 4))
;Value: p2

(x-point p1)
;Value: 1

(y-point p1)
;Value: 2

(x-point p2)
;Value: 3

(y-point p2)
;Value: 4

(define s (make-segment p1 p2))
;Value: s

(start-segment s)
;Value 28: (1 . 2)

(end-segment s)
;Value 29: (3 . 4)

(print-segment s)
(1,2)-(3,4)
;Unspecified return value

(define mid (midpoint-segment s))
;Value: mid

(print-point mid)
(2,3)
;Unspecified return value

