; Exercise 2.71 - Sketch the trees for symbols with these frequencies
(define (generate-freq n)
  (define (generate-iter n items)
    (if (= n 0)
        items
        (generate-iter (- n 1) (cons (expt 2 (- n 1)) items))))
  (generate-iter n `()))

(generate-freq 5)
(generate-freq 10)

; For a general N size,
; Most frequent symbol can be retrieved using 1 bit.
; The least frequenct symbol can be retrieved using (n - 1) bits.

; Exercise 2.72 - Analyzing the order of growth
; The encoding procedure will take O(n^2) in worst case (least frequent items) as we need to scan through the list of symbols and search through the Huffman tree.

; Specific case: exercise 2.71
; O(1) for the most frequent item
; O(n) for the least frequent item