(define rock-songs '((A 2)    (NA 16)
		     (BOOM 1) (SHA 3)
		     (GET 2)  (YIP 9)
		     (JOB 2)  (WAH 1)))

(define rock-tree (generate-huffman-tree rock-songs))

(define rock-message '(GET A JOB
		       SHA NA NA NA NA NA NA NA NA
		       GET A JOB
		       SHA NA NA NA NA NA NA NA NA
		       WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
		       SHA BOOM))

(define rock-enc (encode rock-message rock-tree))
;R. => (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

; Number of bits to store the encoded message
(length rock-enc) ;=> 84

(decode rock-enc rock-tree)
;Result => (get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom)

; Number of bits to encode 8 symbols with fixed length codes = log2 8
(/ (log 8) (log 2)) ;=> 3

; Number of bits to encode the message with fixed length = length * num-bits
(* (length rock-message) 3) ;=> 108
