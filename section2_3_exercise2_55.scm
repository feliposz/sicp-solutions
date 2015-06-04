(car ''abracadabra) ;=> quote

; Internally, this is the same as:

(car (quote (quote abracadabra)))

; The outer quote is creating a list of 2 symbols, that are:
; (quote abracadabra)

; Then the car procedure returns the first element of the list,
; wich is the symbol quote.