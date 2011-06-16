(define nil '())

(define (make-vect x y) (list x y))

(define (vect-x vec) (car vec))

(define (vect-y vec) (cadr vec))

(define (add-vect u v)
  (make-vect (+ (vect-x u) (vect-x v))
             (+ (vect-y u) (vect-y v))))

(define (make-ship position velocity num-torps)
  (define (move) 
    (set! position (add-vect position velocity))
    'done)
  (define (fire-torp)
    (cond ((> num-torps 0)
	   (set! num-torps (- num-torps 1))
	   (let ((torp (make-torpedo position
				     (add-vect velocity velocity))))
	     (add-to-universe torp)
	     'torp-fired))
	  (else 'out-of-torpedoes!)))
  (define (explode ship)
    (newline)
    (display "Ouch. That hurt.")
    (remove-from-universe ship))
  (lambda (msg . args)
    (cond ((eq? msg 'position) position)
	  ((eq? msg 'velocity) velocity)
	  ((eq? msg 'move) (move))
	  ((eq? msg 'attack) (fire-torp))
	  ((eq? msg 'explode) (explode (car args)))
	  ((eq? msg 'clock-tick) (move) (fire-torp))
	  ((eq? msg 'display)
	   (newline)
	   (display "ship at ")
	   (display position))
	  (else (error "ship can't" msg)))))


(define (make-station position)
  (define (explode station)
    (newline)
    (display "a small crack starts tearing the station apart ..."))
  (lambda (msg . args)
    (cond ((eq? msg 'position) position)
	  ((eq? msg 'clock-tick) 'done)
	  ((eq? msg 'explode) (explode (car args)))
	  ((eq? msg 'display)
	   (newline)
	   (display "station at ")
	   (display position))
	  (else (error "station can't " msg)))))

(define *universe* nil)

(define (add-to-universe thing)
  (set! *universe* (cons thing *universe*)))

(define (remove-from-universe thing)
  (set! *universe* (delq thing *universe*)))

(define (clock)
  (for-each (lambda (x) (x 'clock-tick)) *universe*)
  (for-each (lambda (x) (x 'display)) *universe*)
  (let ((collisions (find-collisions *universe*)))
    (for-each (lambda (x) (x 'explode x))
	      collisions)))

(define (run-clock n)
  (cond ((= n 0) 'done)
	(else (clock)
	      (run-clock (- n 1)))))

(define (square x) (* x x))

(define (find-distance a b)
  (sqrt (+ (square (- (vect-x a) (vect-x b)))
	   (square (- (vect-y a) (vect-y b))))))

(define (find-collisions lst)
  (define (test current rest)
    (cond ((null? rest) nil)
	  ((< (find-distance ((car rest) 'position)
			     (current 'position))
	      1)
	   (cons (car rest) (test current (cdr rest))))
	  (else (test current (cdr rest)))))
  (cond ((null? lst) nil)
	(else (let ((new (test (car lst) (cdr lst))))
		(if new ; there are some
		    (append (cons (car lst) new)
			    (find-collisions (cdr lst)))
		    (find-collisions (cdr lst)))))))


(define (make-torpedo position velocity)
  (define (explode torp)
    (newline)
    (display "torpedo goes off!")
    (remove-from-universe torp))
  (define (move) 
    (set! position (add-vect position velocity)))
  (lambda (msg . args)
    (cond ((eq? msg 'position) position)
	  ((eq? msg 'velocity) velocity)
	  ((eq? msg 'move) (move))
	  ((eq? msg 'clock-tick) (move))
	  ((eq? msg 'explode) (explode (car args)))
	  ((eq? msg 'display)
	   (newline)
	   (display "torpedo at ")
	   (display position))
	  (else (error "no method" msg)))))

