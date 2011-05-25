(define (lookup given-key set)
  (cond ((null? set) #f) ; Key not found in set, return false
	(else
	 (let ((entry-key (key (entry set))))
	   (cond ((= given-key entry-key)
		  ; Key found, return record
		  (entry-set))
		 ((< given-key entry-key)
		  ; Key is smaller than current key, look on left branch
		  (lookup given-key (left-branch set)))
		 ((> given-key entry-key)
		  ; Key is greater than current key, look on right branch
		  (lookup given-key (right-branch set))))))))

