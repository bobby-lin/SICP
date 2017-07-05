; Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? coin-values)
  (if (null? coin-values)
      #t
      #f))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(cc 100 us-coins)
  
; Exercise 2.20 - Using dotted-tail notation in filtering parity example

(define (filter-parity n parity items)
  (define (filter-list new-list items)
    (cond ((null? items) new-list)
          ((= (modulo (car items) 2) parity) (filter-list (append new-list (list (car items))) (cdr items)))
          (else (filter-list new-list (cdr items)))))
  (filter-list (list n) items))

(define even-mod-result 0)
(define odd-mod-result 1)

(define (same-parity n . items)
  (if (= (modulo n 2) 0)
      (filter-parity n even-mod-result items)
      (filter-parity n odd-mod-result items)
      ))

(same-parity 1 2 3 4 5 6 7 8 9 10 11 12 13 14 16 17 19 22 52)
(same-parity 2 3 4 5 6 7 8)