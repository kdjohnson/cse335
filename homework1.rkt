#lang racket
(* (+ 3 3) 9)
(/ (* 6 9) (+ (+ 4 2) (* 4 3)))
( * 2 (- 20 (/ 91 7)) (- 45 42))

(define x 2)
(define y 3)
(define z 4)

(define (min-335 x y z)
  (if (and (<= x y)
           (<= x z))
           x
           (if (<= y z)
               y
               z)))

(define (sum2-largest x y z)
  (if (and (= x y) (= y z))
      0
      (if (and (> x y) (< y z))
          (+ x z)
          (+ y z))
      ))