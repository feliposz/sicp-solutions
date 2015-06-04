;symbol,number->Leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

;Object->boolean
(define (leaf? object)
  (eq? (car object) 'leaf))

;Leaf->symbol
(define (symbol-leaf x) (cadr x))

;Leaf->number
(define (weight-leaf x) (caddr x))

;Tree,Tree->Tree
(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

;Tree->Tree
(define (left-branch tree) (car tree))

;Tree->Tree
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

;Leaf,Set<Leafs>|nil->Set<Leafs>
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

;List<pairs>->Set<Leafs>
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)   ; symbol
			       (cadr pair)) ; frequency
		    (make-leaf-set (cdr pairs))))))

;List<symbols>,Tree->List<numbers>
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

;symbol,Tree->List<numbers>
(define (encode-symbol symbol tree)
  (define (encode-1 subtree)
    (if (leaf? subtree)
	'()
	(let ((left (left-branch subtree))
	      (right (right-branch subtree)))
	  (cond ((element-of-list? symbol (symbols left))
		 (cons 0 (encode-1 left)))
		((element-of-list? symbol (symbols right))
		 (cons 1 (encode-1 right)))
		(else (error "invalid symbol to encode ENCODE-SYMBOL:" symbol))))))
  (encode-1 tree))

;symbol,List->boolean
(define (element-of-list? x list)
  (cond ((null? list) #f)
	((eq? x (car list)) #t)
	(else (element-of-list? x (cdr list)))))

;List->Tree
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;Set->Tree
(define (successive-merge leaf-set)
  ;(display leaf-set) (newline)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (let ((left (cadr leaf-set))
	    (right (car leaf-set)))
	(let ((node (make-code-tree left right)))
	  (successive-merge (adjoin-set node (cddr leaf-set)))))))


