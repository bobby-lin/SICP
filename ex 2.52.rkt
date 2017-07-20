;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |ex 2.52|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Exercise 2.52

(require sicp-pict)

; a) Working at lower level to change wave

(define wave-segment (list (make-segment (make-vect 0.5 0.3) (make-vect 0.38 0.0))
                   (make-segment (make-vect 0.5 0.3) (make-vect 0.62 0.0))
                   (make-segment (make-vect 0.82 0.0) (make-vect 0.62 0.45))
                   (make-segment (make-vect 0.62 0.45) (make-vect 0.99 0.22))
                   (make-segment (make-vect 0.99 0.40) (make-vect 0.80 0.62))
                   (make-segment (make-vect 0.80 0.62) (make-vect 0.64 0.62))
                   (make-segment (make-vect 0.64 0.62) (make-vect 0.70 0.82))
                   (make-segment (make-vect 0.70 0.82) (make-vect 0.64 0.99))
                   (make-segment (make-vect 0.44 0.62) (make-vect 0.38 0.82))
                   (make-segment (make-vect 0.38 0.82) (make-vect 0.44 0.99))
                   (make-segment (make-vect 0.55 0.65) (make-vect 0.44 0.72))
                   (make-segment (make-vect 0.55 0.65) (make-vect 0.64 0.72)) ; Smile
                   (make-segment (make-vect 0.44 0.62) (make-vect 0.30 0.62))
                   (make-segment (make-vect 0.30 0.62) (make-vect 0.18 0.54))
                   (make-segment (make-vect 0.18 0.54) (make-vect 0.0 0.85))
                   (make-segment (make-vect 0.18 0.30) (make-vect 0.0 0.60))
                   (make-segment (make-vect 0.18 0.30) (make-vect 0.3 0.51))
                   (make-segment (make-vect 0.3 0.51) (make-vect 0.35 0.40))
                   (make-segment (make-vect 0.35 0.40) (make-vect 0.25 0.0))))

(define wave (segments->painter wave-segment))

(define (right-split painter n)
   (if (= n 0)
       painter
       (let ((smaller (right-split painter (- n 1))))
         (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; b) Working at middle level to modify corner-split

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))

(paint (corner-split wave 2))

; c) Working at highest level to modify square-limit to make the bigger picture look outward from each corner

(define (rotate-90 painter)
  ((transform-painter (make-vect 1.0 0.0)
                      (make-vect 1.0 1.0)
                      (make-vect 0.0 0.0))
  painter))

(define (n-rotate-90 painter n)
  (if (= n 0)
      painter
      (n-rotate-90 (rotate-90 painter) (- n 1))))

(define (flip-horiz-180 painter)
  (n-rotate-90 painter 2))

(define (flip-horiz-270 painter)
  (n-rotate-90 painter 3))

(define (square-limit painter n)
  (let ((quarter (flip-vert (flip-horiz (corner-split painter n)))))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit wave 2))