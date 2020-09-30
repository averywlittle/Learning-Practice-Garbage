#lang racket
; 1.3.1 Procedures as arguments

; sum integers
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

; cube helper
(define (cube x) (* x x x))

; sum cubes
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

; pi sum
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

; These three procedures clearly share a common underlying pattern.
; They are for the most part identical, differing only in the name of the procedure,
; the function of 'a' used to compute the term to be added, and the function that provides the next value of 'a'.
; We can generate each of these procedures using a template.
; The presence of such a common pattern is strong evidence that there is a useful abstraction waiting to be brought
; to the surface.

; Summation of series! The concept is expresssed using Sigma notation.

; A: lower bound
; B: upper bound
; TERM: procedure
; NEXT: procedure
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x) (+ x 1))

; sum cubes
(define (sum-cubes-abstract a b)
  (sum cube a inc b))

(sum-cubes-abstract 1 10)

(define (identity x) x)
(define (sum-integers-abstract a b)
  (sum identity a inc b))

(sum-integers-abstract 1 10)

; pi sum
(define (pi-sum-abstract a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; Now that we have sum, we can use it as a building block in formulating
; other further concepts. For instance, definite integrals.

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

; Exercise 1.29 Simpson's rule compared to definite integral
(define (simp f a b n)
  (define h (/ (- b a) n))
  (define (add-two-h x) (+ x (* 2 h)))
  (* (/ h 3.0) (+ (f a)
                  (* 4.0 (sum f (+ a h) add-two-h b))
                  (* 2.0 (sum f (add-two-h a) add-two-h (- b h)))
                  (f b))))

(simp cube 0 1 100)
(simp cube 0 1 1000)

; ITERATIVE SUM
; A: lower bound
; B: upper bound
; TERM: procedure
; NEXT: procedure
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
     
; A: lower bound
; B: upper bound
; TERM: procedure
; NEXT: procedure
(define (product term a next b)
  (if (> a b)
      1 ; multiplying by 0 will always produce 0, must be 1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(factorial 4)

; Pi approximations
; Set a new term to be used in product
(define (pi-term n)
  (if (even? n)
      (/ (+ n 2.0) (+ n 1.0))
      (/ (+ n 1.0) (+ n 2.0))))

(* 4.0 (product pi-term 1.0 inc 6.0))
(* 4.0 (product pi-term 1.0 inc 100.0))

; Product iter
(define (product-iter term a next b)
  (define (iter a result)
    (if (a > b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

; Abstract product and sum to an even higher order of accumulator 1.32
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-recur combiner null-value (next a) next b))))

; Filter concept introduced
; Can obtain an even more general version of accumulate
(define (fitlered-accumulate-iter filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))
         