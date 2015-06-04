; Changed internal representation of Point to make it more interesting (Check make-rect also)

; int, int -> Point
(define (make-point x y)
  (list y x))

; Point -> int
(define (x-point p)
  (cadr p))

; Point -> int
(define (y-point p)
  (car p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Changed complety the internal representation of a Rect and it's selectors
(define (make-rect corner1 corner2)
  (cons (cons (x-point corner1) (x-point corner2))
	(cons (y-point corner1) (y-point corner2))))

(define (corner1-rect r)
  (make-point (car (car r)) (car (cdr r))))

(define (corner2-rect r)
  (make-point (cdr (car r)) (cdr (cdr r))))

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

; Functions still work. Abstraction is ok!

