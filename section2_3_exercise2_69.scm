(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (display leaf-set) (newline)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (let ((left (cadr leaf-set))
	    (right (car leaf-set)))
	(let ((node (make-code-tree left right)))
	  (successive-merge (adjoin-set node (cddr leaf-set)))))))

; TESTS

(define some-pairs '((A 4) (B 2) (C 1) (D 1)))

(make-leaf-set some-pairs)

(generate-huffman-tree some-pairs)

