; Exercise 2.17 - Procedure to return last item in the list
(define (last-pair list)
  (define (list-rec list)
    (if (null? (cdr list))
        list
        (list-rec (cdr list))))
  (list-rec list))

(last-pair (list 1 2 3 99 11111 2222 888888))

; Exercise 2.18 - Reverse a list
(define (reverse-mod items)
  (define (reverse-iter items)
    (if (null? (cdr items))
        items
        (append (reverse-iter (cdr items)) (list (car items)))))
  (reverse-iter items))
        
(reverse-mod (list 1 4 9 16 25))