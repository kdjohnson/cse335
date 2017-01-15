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
  (cond
    [(and (= x y) (= y z)) 0]
    [(or (and (>= x y) (> y z)) (and (>= y x) (>= x z))) (+ x y)] ;The cases x >= y > z or y >= x > z
    [(or (and (>= x z) (> z y)) (and (>= y z) (>= z x))) (+ y z)] ;The cases x >= z > y or z >= x > y
    [(or (and (>= y z) (> z x)) (and (>= z y) (>= y x))) (+ y z)] ;The cases y >= z > x or z >= y > x
    )
  )

#|
Evaluate a similar if (or cond) expression that has as its value the sum of the two
smallest of x, y and z, if all three are of the same value, the value of the function
is 0 (zero).
(define (sum-two-smallest x y z)
  (if (and (= x y) (= y z))
      0
      (if (and (< z y) (< y x ))
          (+ z y)
          (if (and (< x y) (< y z))
              (+ x y)
              (if (and (< y x) (< x z))
                  (+ y x)
                  (+ x z)
                  )
              )
          )
      )
  )
|#

(define (sum-two-smallest x y z)
  (cond
    [(and (= x y) (= y z)) 0]
    [(or (and (<= x y) (< y z)) (and (<= y x) (<= x z))) (+ x y)] ;The cases x <= y < z or y <= x < z
    [(or (and (<= x z) (< z y)) (and (<= y z) (<= z x))) (+ y z)] ;The cases x <= z < y or z <= x < y
    [(or (and (<= y z) (< z x)) (and (<= z y) (<= y x))) (+ y z)] ;The cases y <= z < x or z <= y < x
    )
  )


#|
Without using an if (or cond) expression, write an expression that uses both
variables x and y, which has as its value #t if the values of x and y are equal,
and #f otherwise.
|#
(define (equals x y)
  (equal? x y)
  )

#|
Q: Experiment with ' (quote) in the Scheme interpreter.  Try, for example 'name, '+,
'(/ 4 2), 'gargleblaster, and unquoted versions of expressions.
In English, give a precise answer the following question: what does ' do?
|#

#|
A: This produces a literal so instead referencing a variable that was declared as
name it will instead will print a literal. 
|#

#|
A similar procedure to ' (quote) is list. What is the difference between ' and list?
Hint: try using variables and function calls in conjunction with the two.
|#

#|
A: The difference is that list will actually create a datay type the holds the
elements that are given to it. While the ' creates a literal representation of what is
given to it. 
|#


#|
What can you do with a string in Scheme that you can't do with a symbol?  
Why is there a distinction between strings and symbols in Scheme?   
(Hint: look at the Revised Report on Scheme, the url of which is given
in sep-9 course slides , to see the operations that Scheme defines for these
types.)
|#

#|A:
You can append a string while you cannot do so with a symbol. The difference is there
since while two strings can have the same contents but not be the same objects. Thus, not being
equal. On the other hand two symbols that have the same contents are guaranteed to be equal. 
|#

#|
Using *only* the Scheme procedure 'list' and numbers and quoted symbols (such as '*, 'name, and 'james), 
write Scheme expressions to make the following lists.

(Note that the line breaks and indentation in the last of these lists do not matter;
the interpreter will print out the value of your answer without this indentation,
which is OK.  We are asking for you to create the values displayed below, not the
printing displayed below.  So do NOT use \newline or #\space in your answer.)
 
(4 2 6 9)

(spaceship
       (name(serenity))
       (class(firefly)))

(2 * ((20 - (91 / 7)) * (45 - 42)))
|#

(list 4 2 6 9)
`(spaceship(name(serenity))(class(firefly)))
`(2 * ((20 - (91 / 7)) * (45 - 42)))


#|
Suppose we are writing code for a procedure in which the variable "lst" is bound to a
list of numbers.  Suppose further that lst has the value:

    (a b c)
For each of the following, write an expression that uses lst and makes the given list:

    (d a b c)
    (a b d a b)
    (b c d a)

Hint:
use the functions cons, car and cdr or other list accessor shorthands
found here:
http://docs.racket-lang.org/reference/pairs.html?q=cadr&q=rackunit#(part._.Pair_.Accessor_.Shorthands)
|#

(define lst (list 1 2 3))
(cons 4 (cons (car lst) (cdr lst)))
(append (reverse (cdr (reverse lst))) (list 4) (reverse (cdr (reverse lst))))
(append (cdr lst) (list 4 (car lst)))

#|
The standard library provides two functions eq? and equal? (the question mark
is part of the name). Experiment with these two functions and concisely describe
the difference between them. 
|#

#|
Per documentation, eq? will return #t if v1 and v2 refer to the same object in memory.
While equal? is a superset of eq? which checks for if list and other such data types
have the same elements. 
|#

#|
Using string manipulation function like: string-append, number->string and symbol->string to:
Define the function create-error-msg that behaves in the following manner:

(create-error-msg 'answer-to-everything 42)
"This is a custom error message we will be using next. Symbol 'answer-to-everything was not paired with value 42"

You may assume that the first argument is always a symbol and that the second one is
always a number.
|#

(define (create-error-msg sym val)
  (string-append "This is a custom error message we will be using next. Symbol '"
                 (symbol->string sym) " was not paired with value " (number->string val)
                 )
  )

#|
For testing purposes we will use exceptions instead of errors (as you might notice
from your textbook). We will not go into detail of how to handle them; all you need to know is that you 
"raise" an exception with the following statement:

> (raise 42)
uncaught exception: 42

> (raise '(42 42))
uncaught exception: '(42 42)

> (raise "forty two")
uncaught exception: "forty two"

Where the first argument can be any Scheme value: number, list, string, to name a few. In later homework,
you will be asked to write functions that result in specific error messages and we will use the value
of the parameter to automatically check the intended semantics.

----DO THE FOLLOWING---
Write a function, check-correctness, that takes a 2 element list, where the 1st element is a symbol and 
the 2nd is a number. (From here on, a 2 element list is referred to as a pair or 2-tuple) . 

The following contract should be enforced by your function: if the symbol is 'answer-to-everything but the 
number is not 42, then raise an exception with the error message that you create with the function defined 
for the previous question; your function returns #t only when the pair is exactly '(answer-to-everything 42);
all other cases your function should return #f.


>(check-correctness '(answer-to-everything 42))
#t

>(check-correctness '(symbol-other-than-the-previous-one 42))
#f

>(check-correctness '(test 30))
#f

>(check-correctness '(answer-to-everything 10))
uncaught exception: "This is a custom error message we will be using next. Symbol
'answer-to-everything was not paired with value 42"
|#
      