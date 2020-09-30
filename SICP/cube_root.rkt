#lang racket

(define (square guess) (* guess guess))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (= (improve guess x) guess))

(define (3rt-iter guess x)
  (if (good-enough? guess x)
      guess
      (3rt-iter (improve guess x) x)))

(define (cube-root x) (3rt-iter 1.1 x))
         
(cube-root 27)

(cube-root 0)