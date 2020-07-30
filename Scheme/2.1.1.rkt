#lang racket

; The pair
(define x (cons 1 2))
(car x)
(cdr x)

; Can redefine these procedures to better represent what they do: make-rat, numer, and denom
(define (pair a b)
  (cons a b))
(define (numer x) (car x))
(define (denom x) (cdr x))

; GCD
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Change make-rat to produce the lowest terms before constructing the pairs. Need gcd
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (make-rat-plain n d) (cons n d))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-third (make-rat 1 3))
(print-rat one-third) ; I was calling this with too many parenteses


; Make rat that handles negatives
(define (make-rat-handle-negatives n d)
  (let ((g (gcd n d)))
    (if (or (and (> n 0) (> d 0)) (and (< n 0) (< d 0))) ; If both pos or both neg
         (cons (/ n g) (/ d g)) ; Perform operation
         (cons (* -1 (/ n g)) (* -1 (/ d g)))) ; Otherwise, only make numerator negative
    ))

(define negative-rat (make-rat-handle-negatives -2 4))
(print-rat negative-rat)

; 2.1.2
; Let's further abstract from the pair. Can we represent a line on a plane?
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (define (average a b) (/ (+ a b) 2.0))
  (let ((a (start-segment segment))
        (b (end-segment segment)))
    (make-point (average (x-point a)
                         (x-point b))
                (average (y-point a)
                         (y-point b)))))


; Testing 
(define seg (make-segment (make-point 2 3) 
                          (make-point 10 15))) 
  
(print-point (midpoint-segment seg)) 

; RECTANGLES, will utilize segment setup from exercise 2.2
; Represent using two points
(define (make-rect bottom-left top-right)
  (cons bottom-left top-right))

; Accessors
(define (bottom-left rect) (car rect))
(define (bottom-right rect)
  (make-point (x-point (cdr rect))
              (y-point (car rect))))
(define (top-left rect)
  (make-point (x-point (car rect))
              (y-point (cdr rect))))
(define (top-right rect) (cdr rect))

(define (width-rect rect)
  (abs (- (x-point (bottom-left rect)
          (x-point (bottom-right rect))))))

(define (height-rect rect)
  (abs (- (y-point (bottom-left rect)
          (y-point (top-left rect))))))

; Perimeter
(define (perimeter rect)
  (+ (* 2 (width-rect rect)) (* 2 (height-rect rect))))

; Area
(define (area rect)
  (* (width-rect rect) (height-rect rect)))

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

(car (cons 5 7))








