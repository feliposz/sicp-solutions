(define square-shape
  (make-picture
   (list
    (make-segment (make-vect 0 0) (make-vect 0 1))
    (make-segment (make-vect 0 1) (make-vect 1 1))
    (make-segment (make-vect 1 1) (make-vect 1 0))
    (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define big-x
  (make-picture
   (list
    (make-segment (make-vect 0 0) (make-vect 1 1))
    (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define diamond
  (make-picture
   (list
    (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
    (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
    (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
    (make-segment (make-vect 0.5 1) (make-vect 0 0.5)))))

(graphics-clear gdev)
(square-shape gdev-rect)
(big-x gdev-rect)
(diamond gdev-rect)

; Define the points of george, used in constructing the line segments
(define p1 (make-vect .25 0))
(define p2 (make-vect .35 .5))
(define p3 (make-vect .3 .6))
(define p4 (make-vect .15 .4))
(define p5 (make-vect 0 .65))

(define p6 (make-vect .4 0))
(define p7 (make-vect .5 .3))
(define p8 (make-vect .6 0))

(define p9 (make-vect .75 0))
(define p10 (make-vect .6 .45))
(define p11 (make-vect 1 .15))

(define p12 (make-vect 1 .35))
(define p13 (make-vect .75 .65))
(define p14 (make-vect .6 .65))
(define p15 (make-vect .65 .85))
(define p16 (make-vect .6 1))

(define p17 (make-vect .4 1))
(define p18 (make-vect .35 .85))
(define p19 (make-vect .4 .65))
(define p20 (make-vect .3 .65))
(define p21 (make-vect .15 .6))
(define p22 (make-vect 0 .85))

; Describe george as a list of line segments
(define george-lines
  (list (make-segment p1 p2)
	(make-segment p2 p3)
	(make-segment p3 p4)
	(make-segment p4 p5)

	(make-segment p6 p7)
	(make-segment p7 p8)

	(make-segment p9 p10)
	(make-segment p10 p11)

	(make-segment p12 p13)
	(make-segment p13 p14)
	(make-segment p14 p15)
	(make-segment p15 p16)

	(make-segment p17 p18)
	(make-segment p18 p19)
	(make-segment p19 p20)
	(make-segment p20 p21)
	(make-segment p21 p22)))

(define g (make-picture george-lines))
