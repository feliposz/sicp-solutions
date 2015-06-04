(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define s (cons-stream 1 (add-streams s s)))
; 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 ... 
