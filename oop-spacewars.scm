; This is a skeleton of a space-wars simulation game as shown at:
; http://icampustutor.csail.mit.edu/6.001-public/tutor.cgi
; Chapter 14: Object-Oriented Programming

; Vector tagged data

(define (square x) (* x x))

(define (make-vect x y)
  (list 'vect x y))
(define (vect? v)
  (and (pair? v) (eq? (car v) 'vect)))
(define (vect-x v)
  (if (vect? v)
      (cadr v)
      (error "Not a vector -- VECT-X" v)))
(define (vect-y v)
  (if (vect? v)
      (caddr v)
      (error "Not a vector -- VECT-X" v)))
(define (add-vect v1 v2)
  (if (and (vect? v1) (vect? v2))
      (make-vect (+ (vect-x v1) (vect-x v2))
                 (+ (vect-y v1) (vect-y v2)))
      (error "Not vectors -- ADD-VECT" v1 v2)))
(define (dist-vect v1 v2)
  (if (and (vect? v1) (vect? v2))
      (sqrt (+ (square (- (vect-x v1) (vect-x v2)))
               (square (- (vect-y v1) (vect-y v2)))))
      (error "Not vectors -- DIST-VECT" v1 v2)))
  
; Class definitions

(define (make-ship position velocity num-torps)
  (define (move)
    (set! position (add-vect position velocity))
    'done)
  (define (fire-torp)
    (cond ((> num-torps 0)
           (set! num-torps (- num-torps 1))
           (display "Fire!")
           (newline))
          (else 'FAIL)))
  (define (draw)
    (display "Ship at ")
    (display position)
    (display " moving by ")
    (display velocity)
    (display " carrying ")
    (display num-torps)
    (display " torpedoes")
    (newline))  
  (define (explode ship)
    (display "Ship exploded at")
    (display position)
    (newline)
    (remove-from-universe ship))
  (define (dispatch msg . args)
    (cond ((eq? msg 'POSITION) position)
          ((eq? msg 'VELOCITY) velocity)
          ((eq? msg 'MOVE) (move))
          ((eq? msg 'ATTACK) (fire-torp))
          ((eq? msg 'DISPLAY) (draw))
          ((eq? msg 'EXPLODE) (explode (car args)))
          (else (error "Ship can't" msg))))
  (if (and (vect? position) (vect? velocity) (number? num-torps))
      dispatch
      (error "Can't instantiate ship with these arguments -- MAKE-SHIP"
             position velocity num-torps)))

(define (make-station position)
  (define (draw)
    (display "Station at ")
    (display position)
    (newline))
  (define (dispatch msg)
    (cond ((eq? msg 'DISPLAY) (draw))
          (else (error "Station can't" msg))))
  (if (vect? position)
      dispatch
      (error "Can't instantiate station with these arguments -- MAKE-STATION"
             position)))

(define (make-torpedo position velocity target proximity-fuse)
  (define (move)
    (set! position (add-vect position velocity))
    (if (< (dist-vect position (target 'POSITION)) proximity-fuse)
        (begin
          (explode)
          (target 'EXPLODE target)))        
    'done)
  (define (draw)
    (display "Torpedo at ")
    (display position)
    (display " moving by ")
    (display velocity)
    (newline))
  (define (explode)
    (display "Torpedo exploded at ")
    (display position)
    (newline))
    ;(remove-from-universe torp))
  (define (dispatch msg . args)
    (cond ((eq? msg 'POSITION) position)
          ((eq? msg 'VELOCITY) velocity)
          ((eq? msg 'MOVE) (move))
          ((eq? msg 'DISPLAY) (draw))
          ;((eq? msg 'EXPLODE) (explode (car args)))
          (else (error "Torpedo can't" msg))))
  (if (and (vect? position) (vect? velocity) (procedure? target)
           (number? proximity-fuse))
      dispatch
      (error "Can't instantiate torpedo with these arguments -- MAKE-TORPEDO"
             position velocity)))  

; Instances

(define enterprise
  (make-ship (make-vect 10 10) (make-vect 5 0) 3))
(define falcon
  (make-ship (make-vect -10 10) (make-vect 10 0) 8))
(define babylon3
  (make-station (make-vect 0 0)))
(define t1
  (make-torpedo (make-vect 0 10) (make-vect 7 0) enterprise 2))

(define *the-universe* (list babylon3 enterprise falcon t1))

(define (remove-from-universe object)
  (define (remove-loop list)
    (cond ((null? list)
           (display "Object is not in the universe.")
           (newline)
           '())
          ((eq? object (car list))
           (display "Object removed from universe.")
           (newline)
           (cdr list))
          (else (cons (car list)
                      (remove-loop (cdr list))))))
  (set! *the-universe* (remove-loop *the-universe*)))

(define (display-universe)
  (define (display-loop list)
    (cond ((null? list) 'done)
          (else
           ((car list) 'DISPLAY)
           (display-loop (cdr list)))))
  (newline)
  (display "A view of the universe:")
  (newline)
  (display-loop *the-universe*))

;(init-clock *the-universe*)
;(run-clock 100)

;(display-universe)

(enterprise 'display) (t1 'display)
(enterprise 'move) (t1 'move)
(enterprise 'display) (t1 'display)
(enterprise 'move) (t1 'move)
(enterprise 'display) (t1 'display)
(enterprise 'move) (t1 'move)
(enterprise 'display) (t1 'display)
(enterprise 'move) (t1 'move)
(enterprise 'display) (t1 'display)
(enterprise 'move) (t1 'move)

;(display-universe)