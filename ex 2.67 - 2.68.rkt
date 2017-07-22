; Implementing Huffman Encoding tree

(define (error reason . args)
      (display "Error: ")
      (display reason)
      (for-each (lambda (arg) 
                  (display " ")
		  (write arg))
		args))

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

; Following Huffman algorithm, 0 -> move to left, 1 -> move to right
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      `()
      (let ((pairs (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

; Exercise 2.67 - Using the decoding procedure

(define sample-tree
  (make-code-tree (make-leaf `A 4)
                  (make-code-tree
                   (make-leaf `B 2)
                   (make-code-tree (make-leaf `D 1)
                                   (make-leaf `C 1)))))

(define sample-message `(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ; decoded message is (a d a b b c a)

; Exercise 2.68 - Define encoding procedure

; encode-symbol : return list of bits that encodes a given symbol
; Scenarios:
; Move to Left: Either on leaf or not on leaf
; Move to Right: Either on leaf or not on leaf

(define (encode-symbol symbol tree)
  (define (encode-iter symbol current-branch result)
    (cond ((leaf? current-branch)
           (if (equal? symbol (symbol-leaf current-branch))
               result
               (error "Symbol is not found in the tree -- ENCODE-SYMBOL" symbol)))
          ((and (not (leaf? current-branch)) (equal? symbol (car (symbols current-branch)))) (encode-iter symbol (left-branch current-branch) (append result (list 0))))
          (else (encode-iter symbol (right-branch current-branch) (append result (list 1))))))
  (encode-iter symbol tree `()))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(encode-symbol `a sample-tree) ; (0)
(encode-symbol `b sample-tree) ; (1 0)
(encode-symbol `c sample-tree) ; (1 1 1)
(encode-symbol `d sample-tree) ; (1 1 0)
(encode-symbol `p sample-tree) ; should display error
(newline)
(encode-symbol 1 sample-tree) ; should display error

(define (encode message tree)
  (if (null? message)
      `()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define sample-message-to-encode `(a d a b b c a)) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)
(define diff-msg (list 1 1 1 1 1 1 1 1))
(newline)
(encode sample-message-to-encode sample-tree)
(equal? (encode sample-message-to-encode sample-tree) sample-message)
(equal? (encode sample-message-to-encode sample-tree) diff-msg)