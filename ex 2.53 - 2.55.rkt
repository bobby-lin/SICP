; Exercise 2.53

(list `a `b `c)
; (a b c)
(list (list `george))
; ((george))
(cdr `((x1 x2) (y1 y2)))
; ((y1 y2))
(cadr `((x1 x2) (y1 y2)))
; (y1 y2)
(pair? (car `(a short list)))
; false
(memq `red `((red shoes) (blue socks)))
; false
(memq `red `(red shoes blue socks))
; (red shoes blue socks)

; Exercise 2.54 - implementing equals?

(define (equals? a b)
  (cond ((null? (cdr a)) (null? (cdr b)))
        ((eq? (car a) (car b)) (equals? (cdr a) (cdr b)))
        (else #f)))

(equals? `(this is a list) `(this is a list)) ; true
(equals? `(this is a list) `(this (is a) list)) ; false
(equals? `(this is a list) `(these are lists)) ; false
(equals? `(a b c d e) `(a b c d e)) ; true

; Exercise 2.55 - explain why....
(car ``abracadabra)
(display ``abracadabra) ; `abracadabra
(newline)
(cdr ``abracadabra)
; The interpreter treats the second quote as a symbol.
; So (car ``abracadabra) will retrieve the first symbol (which is a quotation)
      