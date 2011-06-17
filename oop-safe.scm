(load "objsys.scm")

;;; 
;;; Safe (As in -- where you put your valuables) Abstractions
;;;

;; named-object
;;
(define (create-named-object name)      ; symbol -> named-object
  (create-instance make-named-object name))

(define (make-named-object self name)
  (let ((root-part (make-root-object self)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'named-object root-part)))
        ((NAME) (lambda () name))
        (else (get-method message root-part))))))
  
;; container
;;
;; A container holds THINGS.  
;; 
;; This class is not meant for "stand-alone" objects; rather, 
;; it is expected that other classes will inherit from the
;; container class in order to be able to contain things. 
;; For this reason, there is no create-container procedure.

(define (make-container self)
  (let ((root-part (make-root-object self))
        (things nil)) ; a list of THING objects in container
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'container root-part)))
        ((THINGS) (lambda () things))
        ((HAVE-THING?)
         (lambda (thing)  ; container, thing -> boolean
           (if (memq thing things) #t #f)))
        ((ADD-THING)
         (lambda (new-thing)
           (if (not (ask self 'HAVE-THING? new-thing))
               (set! things (cons new-thing things)))
           'DONE))
        ((DEL-THING)
         (lambda (thing)
           (set! things (delq thing things))
           'DONE))
        (else (get-method message root-part))))))

(define (delq item lst)
  (cond ((null? lst) '())
        ((eq? item (car lst)) (delq item (cdr lst)))
        (else (cons (car lst) (delq item (cdr lst))))))

;; safe
;;
;; symbol, number -> safe
(define (create-safe name combo)
  (create-instance make-safe name combo))

(define (make-safe self name combo)
  (let (;; Superclasses
	(named-part (make-named-object self name))
	(container-part (make-container self))
	;; Additional local state
	(locked? #t))          ; boolean
    (lambda (message)
      (case message
        ((TYPE) (lambda ()
		  (type-extend
		   'safe named-part container-part)))
	((LOCKED?) (lambda () locked?))
	((LOCK) (lambda () (set! locked? #t)))
	((UNLOCK) (lambda (purported-combo)
		    (if (= purported-combo combo)
			(begin (set! locked? #f)
			       #t)
			#f)))
	((ADD-THING) ;; return #t if success, else #f
	 (lambda (new-thing)
	   (if (ask self 'LOCKED?)
	       #f
	       (begin
		 (ask container-part 'ADD-THING new-thing)
		 #t))))
	((DEL-THING)  ;; return #t if success, else #f
	 (lambda (old-thing)
	   (if (or (ask self 'LOCKED?)
		   (not (ask self 'HAVE-THING? old-thing)))
	       #f
	       (begin
		 (ask container-part 'DEL-THING old-thing)
		 #t))))
	(else (get-method message named-part container-part))))))

(define (create-limited-capacity-safe name combo capacity)
  (create-instance make-limited-capacity-safe name combo capacity))
(define (make-limited-capacity-safe self name combo capacity)
  (let (;; Superclasses
	(safe-part (make-safe self name combo))
        (items 0))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'limited-capacity-safe safe-part)))
	((ADD-THING) ;; return #t if success, else #f
	 (lambda (new-thing)
	   (cond ((= items capacity) #f)
                 ((ask safe-part 'ADD-THING new-thing)
                  (set! items (+ items 1))
                  #t)
                 (else #f))))
	((DEL-THING)  ;; return #t if success, else #f
	 (lambda (old-thing)
	   (cond ((= items 0) #f)
                 ((ask safe-part 'DEL-THING old-thing)
                  (set! items (- items 1))
                  #t)
                 (else #f))))
	(else (get-method message safe-part))))))


; Tests

(let ((s (create-limited-capacity-safe 'security 123 2)))
  (ask s 'add-thing 'watch))
(let ((s (create-limited-capacity-safe 'security 123 2)))
  (ask s 'unlock 123))
(let ((s (create-limited-capacity-safe 'security 123 2)))
  (ask s 'unlock 123)
  (ask s 'add-thing 'watch)
  (ask s 'things))
(let ((s (create-limited-capacity-safe 'security 123 2)))
  (ask s 'unlock 123)
  (ask s 'add-thing 'watch)
  (ask s 'add-thing 'gold)
  (ask s 'add-thing 'tiara)
  (ask s 'things))
(let ((s (create-limited-capacity-safe 'security 123 2)))
  (ask s 'unlock 123)
  (ask s 'add-thing 'watch)
  (ask s 'add-thing 'gold)
  (ask s 'del-thing 'gold)
  (ask s 'things))
(let ((s (create-limited-capacity-safe 'security 123 2)))
  (ask s 'unlock 123)
  (ask s 'add-thing 'watch)
  (ask s 'add-thing 'gold)
  (ask s 'del-thing 'gold)
  (ask s 'add-thing 'tiara)
  (ask s 'things))
