#lang racket

; Fixed point procedure
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; 1.35 Golden ratio (~1.61)
(fixed-point (lambda (x) (+ 1.0 (/ 1 x)))
             1.0)

; 1.36 Modified fixed-point
(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2.0)

(define (average x y)
  (/ (+ x y) 2.0)) ; The order of operands in division(/) procedures matter
(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             10.0)

; 1.37
(define (cont-frac n d k)
  (if (= k 0.0)
      0
      (/ (n k) (+ (d k) (cont-frac n d (- k 1.0))))))


(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10.0)

; Iter
(define (cont-frac-iter n d k)
  (define (iter term result)
    (if (= term 0.0)
        result
        (iter (- term 1.0) (/ (n term) (+ (d term) result)))))
  (iter k 0.0))

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           12.0)

; 1.38 find e(2.71828)
(define (euler k)
  (+ 2 (cont-frac-iter (lambda (i) 1)
                         (lambda (i)
                           (if (= (remainder i 3) 2)
                               (/ (+ i 1) 1.5)
                               1))
                         k)))

(euler 10.0)

; 1.39 x=rads, k=terms
(define (tan-cf x k)
  (cont-frac-iter (lambda (i)
               (if (= i 1) x (- (* x x))))
             (lambda (i)
               (- (* i 2) 1))
             k))
  


