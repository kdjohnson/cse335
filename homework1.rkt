#lang racket
#|
Translate the following algebraic formulas into Scheme's notation.
Type the translation into Scheme's interaction window for checking.
((3 + 3) * 9)
((6 * 9) / ((4 + 2) + (4 * 3)))       
(2* ((20 - (91 / 7)) * (45 - 42)))
|#
(* (+ 3 3) 9)
(/ (* 6 9) (+ (+ 4 2) (* 4 3)))
( * 2 (- 20 (/ 91 7)) (- 45 42))

#|
Using Scheme's define special form, write definitions of variables x, y and z,
such that x has a value of 2 and y has a value of 3 and z has a value of 4.
|#
(define x 2)
(define y 3)
(define z 4)

#|
Write and evaluate an if (or cond) expression in Scheme that uses
the variables x, y and z, and that has as its value the sum of the
two largest variables, and, if all are of the same value, the value
of the function is 0 (zero).
|#
(define (sum-two-largest x y z)
  (if (and (= x y) (= y z))
      0
      (if (and (> x y) (< y z))
          (+ x z)
          (+ y z)
          )
      )
  )

#|
Evaluate a similar if (or cond) expression that has as its value the sum of the two
smallest of x, y and z, if all three are of the same value, the value of the function
is 0 (zero).
|#
(define (sum-two-smallest x y z)
  (if (and (= x y) (= y z))
      0
      (if (and (< x y) (> y z))
          (+ x z)
          (+ y z)
          )
      )
  )