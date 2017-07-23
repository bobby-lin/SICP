; Huffman tree implementation

(define (make-leaf symbol weight)
  (list `leaf symbol weight))

(define (leaf? object)
  (eq? (car object) `leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (error reason . args)
      (display "Error: ") (display reason)
      (for-each (lambda (arg) (display " ") (write arg)) args))

; Decoding procedure

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        `()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

; Encoding procedure

 (define (encode-symbol symbol tree) 
   (cond 
     ((leaf? tree) '()) 
     ((member symbol (symbols tree)) 
       (let ((left (left-branch tree)) (right (right-branch tree))) 
          (if (member symbol (symbols left)) 
              (cons 0 (encode-symbol symbol left)) 
              (cons 1 (encode-symbol symbol right))))) 
     (else (error "bad symbol -- ENCODE-SYMBOL" symbol)))) 

(define (encode message tree)
  (if (null? message)
      `()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; make-leaf-set transforms list of pairs into ordered set of leaves
(define (make-leaf-set pairs)
  (if (null? pairs)
      `()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair)) ; symbol , frequency
                    (make-leaf-set (cdr pairs))))))

; Exercise 2.69

(define test-pairs (list `(a 8) `(b 5) `(c 2)))
(define test-pairs-more (list `(h 1) `(g 1) `(c 1) `(d 1) `(b 3) `(a 9)))

; successive-merge uses make-code-tree to merge the smallest weight elements of the set until there is only one element left.
; Following Huffman algorithm, the more frequently used symbol should be nearer to the root.

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set))))) ; left, right, rest of the leaf-set

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; Exercise 2.70 - Generating Huffman tree and encode messages

(define list-pairs (list `(A 2) `(BOOM 1) `(GET 2) `(JOB 2) `(NA 16) `(SHA 3) `(YIP 9) `(WAH 1)))
(define huffman-tree (generate-huffman-tree list-pairs))
(define lyric-1 `(Get a job))
(define lyric-2 `(Sha na na na na na na na na))
(define lyric-3 `(Wah yip yip yip yip yip yip yip yip yip))
(define lyric-4 `(Sha boom))
(define lyrics (append lyric-1 lyric-2 lyric-1 lyric-2 lyric-3 lyric-4))
(encode lyrics huffman-tree)

(length (encode lyrics huffman-tree))
; We require 84 bits to encode the lyrics.
; NA: 000
; YIP: 001
; SHA: 010
; A: 011
; GET: 100
; JOB: 101
; BOOM: 110
; WAH: 111
(length lyrics)
; If we use a fixed-length code for the eight symbols, then we need to use at least (36 * 3 = 108) bits