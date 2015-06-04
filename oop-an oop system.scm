; basic object oriented system

(define (make-instance)
  (let ((handler #f))
    (lambda (message)
      (case message
        ((SET-HANDLER!)
         (lambda (handler-proc)
           (set! handler handler-proc)))
        (else (get-method message handler))))))

(define (create-instance maker . args)
  (let* ((instance (make-instance))
         (handler (apply maker instance args)))
    (ask instance 'SET-HANDLER! handler)
    instance))

(define (get-method message object)
  (object message))

; TODO: allow for multiple objects
(define (type-extend type object)
  (cons type (ask object 'TYPE)))

; HACK: is this enough?
(define method? procedure?)

(define (ask object message . args)
  (let ((method (get-method message object)))
    (if (method? method)
        (apply method args)
        (error "No method for message" message))))

(define (make-root-object self)
  (lambda (message)
    (case message
      ((TYPE)
       (lambda () (list 'root)))
      ((IS-A)
       (lambda (type)
         (if (memq type (ask self 'TYPE))
             #t
             #f)))
      (else (error "No method" message "for object" self)))))

; create-book: symbol, number -> book
(define (create-book name copyright)
  (create-instance make-book name copyright))
(define (make-book self name copyright)
  (let ((named-object-part (make-named-object self name)))
    (lambda (message)
      (case message
        ((TYPE)
         (lambda () (type-extend 'book named-object-part)))
        ((YEAR)
         (lambda () copyright))
        (else (get-method message named-object-part))))))

; create-named-object: symbol -> named-object
(define (create-named-object name)
  (create-instance make-named-object name))
(define (make-named-object self name)
  (let ((root-part (make-root-object self)))
    (lambda (message)
      (case message
        ((TYPE)
         (lambda () (type-extend 'named-object root-part)))
        ((NAME)
         (lambda () name))
        ((CHANGE-NAME)
         (lambda (newname) (set! name newname)))
        (else (get-method message root-part))))))

(define felipo (create-named-object 'felipo))
(define sicp (create-book 'scip 2011))

(ask felipo 'type)
(ask sicp 'type)

(ask sicp 'is-a 'book)
(ask sicp 'is-a 'named-object)
(ask felipo 'is-a 'book)
(ask felipo 'is-a 'named-object)
