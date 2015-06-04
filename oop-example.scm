(load "objsys.scm")

; ====================================================
; Student/Professor objects

; create-person: symbol -> person
(define (create-person name)
  (create-instance make-person name))
(define (make-person self name)
  (let ((root-part (make-root-object self)))
    (lambda (message)
      (case message
        ((TYPE)
         (lambda () (type-extend 'person root-part)))
        ((WHOAREYOU?)
         (lambda () name))
        ((SAY)
         (lambda (stuff) stuff))
	((CHANGE-NAME)
	 (lambda (new-name)
	   (set! name new-name)
	   (ask self 'say (append '(call me)
				  (list (ask self 'whoareyou?))))))        
        ((QUESTION)
         (lambda (of-whom query) ; person, list -> list
           (ask of-whom 'answer self query)))
        ((ANSWER)
         (lambda (whom query) ; person, list -> list
           (ask self 'say
                (cons (ask whom 'whoareyou?)
                      (append '(i do not know about)
                              query)))))
        (else (get-method message root-part))))))

; create-professor: symbol -> professor
(define (create-professor name)
  (create-instance make-professor name))
(define (make-professor self name)
  (let ((person-part (make-person self name)))
    (lambda (message)
      (case message
        ((TYPE)
         (lambda () (type-extend 'professor person-part)))
        ((WHOAREYOU?)
         (lambda ()
           (list 'prof (ask person-part 'whoareyou?))))
        ((LECTURE)
         (lambda (notes)
           ;note: self is used instead of person-part, so that the overriden
           ;      say-method is called if implemented by subclasses
           (cons 'therefore (ask self 'say notes))))
        (else (get-method message person-part))))))

; create-arrogant-professor: symbol -> arrogant-professor
(define (create-arrogant-professor name)
  (create-instance make-arrogant-professor name))
(define (make-arrogant-professor self name)
  (let ((prof-part (make-professor self name)))
    (lambda (message)
      (case message
        ((TYPE)
         (lambda () (type-extend 'arrogant-professor prof-part)))
        ((SAY)
         (lambda (stuff)
           (append (ask prof-part 'say stuff) (list 'obviously))))
        ((ANSWER)
         (lambda (whom query)
           (cond ((ask whom 'is-a 'student)
                  (ask self 'say
                       '(this should be obvious to you)))
                 ((ask whom 'is-a 'professor)
                  (ask self 'say
                       (append '(but you wrote a paper about)
                               query)))
                 (else
                  (ask prof-part 'answer whom query)))))
        (else (get-method message prof-part))))))

; create-student: symbol -> student
(define (create-student name)
  (create-instance make-student name))
(define (make-student self name)
  (let ((person-part (make-person self name)))
    (lambda (message)
      (case message
        ((TYPE)
         (lambda () (type-extend 'student person-part)))
        ((SAY)
         (lambda (something)
           (append '(excuse me but) (ask person-part 'say something))))
        (else (get-method message person-part))))))

(define p1 (create-person 'joe))
(ask p1 'whoareyou?)
(ask p1 'say '(the sky is blue))
(newline)

(define prof1 (create-professor 'fred))
(ask prof1 'whoareyou?)
(ask prof1 'lecture '(the sky is blue))
(newline)

(define ap1 (create-arrogant-professor 'perfect))
(ask ap1 'whoareyou?)
(ask ap1 'say '(the sky is blue))
(ask ap1 'lecture '(the sky is blue))
(newline)

(define s1 (create-student 'bert))
(ask s1 'whoareyou?)
(ask s1 'say '(i do not understand))
(newline)

(ask s1 'question p1 '(why is the sky blue))
(ask p1 'question ap1 '(why so serious))
(ask prof1 'question ap1 '(why so serious))
(ask s1 'question ap1 '(why so serious))
(newline)

(ask s1 'change-name 'whatever)
(newline)

(ask p1 'type)
(ask prof1 'type)
(ask ap1 'type)
(ask s1 'type)

