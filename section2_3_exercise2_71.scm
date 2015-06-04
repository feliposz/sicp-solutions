(define tree5 (generate-huffman-tree '((a 1) (b 2) (c 4) (d 8) (e 16))))

(length (encode-symbol 'a tree5)) ;=> 4 bits to encode the least frequent symbol if n=5
(length (encode-symbol 'e tree5)) ;=> 1 bit to encode the most frequent symbol if n=5

(define tree10 (generate-huffman-tree '((a 1) (b 2) (c 4) (d 8) (e 16) (f 32) (g 64) (h 128) (i 256) (j 512))))

(length (encode-symbol 'a tree10)) ;=> 9 bits to encode the least frequent symbol if n=10
(length (encode-symbol 'j tree10)) ;=> 1 bit to encode the most frequent symbol if n=10

; In general, the number of bits required for the least frequent symbol is n-1
; For the most frequent, 1 bit is required