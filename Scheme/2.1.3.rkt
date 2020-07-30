#lang racket

; Representing pairs procedurally

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

(car (cons 5 7))
(cdr (cons 5 7))

; Represent pairs of non-negative integers using only numbers and arithmetic operations
(define (int-pair a b)
  (* (expt 2.0 a) (expt 3.0 b)))

; count-0-remainder-divisions divides pair by divider (comes from our definition of the paid pair=2^a*3^b
; will divide until pair is no longer a multiple of d. will return number of divisions
(define (count-0-remainder-divisions n pair divisor)
  (if (= (remainder pair divisor) 0)
         (count-0-remainder-divisions (+ n 1) (/ pair divisor) divisor)
         n))

; It's easy to isolate a and b, but the other value is missing
(define (int-car c) ; will be even
  (count-0-remainder-divisions 0 c 2))
(define (int-cdr c) ; will be odd
  (count-0-remainder-divisions 0 c 3))

(define one-zero-eight (int-pair 2.0 3.0)) ; a: 2, b: 3
(display one-zero-eight)
(newline)
(int-car one-zero-eight)
(int-cdr one-zero-eight)

; Exercise 2.6
; We can define numbers with purely procedures
; damn


(define (square x) (* x x))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

((zero square) 3)
((one square) 3)
((two square) 3)

; Alyssa's intervals
(define (make-interval a b) (cons a b))
(define (upper-bound c) (car c))
(define (lower-bound c) (cdr c))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Conditions are simple, but racket uses errors so I'm just going to skip this. It's just a print line statement really
;(define-condition on-division-by-zero (error)
  ;((message :initarg :message :reader message)))

(define (div-interval x y)
  ;(if (and (= x 0) (= y 0)) (error))
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

; 2.9
; For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of
; the argument intervals, whereas for others the width of the combination is not a function of the widths of the
; argument intervals. Show the width of the sum or difference of two intervals is a function only of the widths of the
; intervals being added. Give exmaples to show this is not true for multiplication or division.
; answer written in notebook

; 2.11
; “By testing the signs of the endpoints of the intervals, it is
; possible to break mul-interval into nine cases, only one
; of which requires more than two multiplications.”

(define (mul-interval-ben x y)
  



