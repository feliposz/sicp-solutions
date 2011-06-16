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

; create-singer: (no-args) -> singer
(define (create-singer)
  (create-instance make-singer))
(define (make-singer self)
  (let ((root-part (make-root-object self)))
    (lambda (message)
      (case message
        ((TYPE)
         (lambda () (type-extend 'singer root-part)))
        ((SAY)
         (lambda (stuff)
           (append stuff '(tra la la))))
        ((SING)
         (lambda ()
           (ask self 'say '(the hills are alive))))
        (else (get-method message root-part))))))

; create-singing-arrogant-prof: symbol -> singing-arrogant-prof
(define (create-singing-arrogant-prof name)
  (create-instance make-singing-arrogant-prof name))
(define (make-singing-arrogant-prof self name)
  (let ((arr-prof-part (make-arrogant-professor self name))
        (singer-part (make-singer self)))
    (lambda (message)
      (case message
        ((TYPE)
         (lambda () (type-extend 'singing-arrogant-prof
                                 singer-part arr-prof-part)))
        (else (get-method message singer-part arr-prof-part))))))

(define sap1 (create-singing-arrogant-prof 'zoe))
(ask sap1 'type)
(ask sap1 'whoareyou?)
(ask sap1 'sing)
(ask sap1 'say '(the sky is blue))
(ask sap1 'lecture '(the sky is blue))