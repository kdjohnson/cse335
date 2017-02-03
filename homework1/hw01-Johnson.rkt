#lang racket
(#%provide (all-defined))

#|
If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:
   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the names of any definition
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     but you can change the names of the arguments if you deem it necessary.
   - make sure that you submit an asnwer sheet that compiles! If you cannot write
     a correct solution at least make it compile, if you cannot make it compile then
     comment it out. In the latter case, make sure that the default definitions
     for the problem are still present. Otherwise you may be penalized up to 25%
     of the total points for the homework.
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. You will
lose up to 25% of the total points for the entire homework depending on the number of errors.
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!

|#
;======================================01=======================================
;((3 + 3) * 9)
;equal to 54
(define (p1-1)
(* (+ 3 3) 9)
)

;((6 * 9) / ((4 + 2) + (4 * 3)))
;equal to 3
(define (p1-2)
(/ (* 6 9) (+ (+ 4 2) (* 4 3)))
)

;(2* ((20 - (91 / 7)) * (45 - 42)))
;equal to 42
(define (p1-3)
( * 2 (- 20 (/ 91 7)) (- 45 42))
)
;======================================02=======================================
;write your answer as a string; you do not need to write any special escape
;characters to distinguish new lines.
(define p2
  "To convert from an algebraic formula into Scheme's notation you could use
a prefix tree to convert. In this format (root left-child right-child). With root being
the top level operator and left-child and right-child being the operands"
)
;======================================03=======================================
;;Write the definitions of x,y,z here:
(define x 2)
(define y 3)
(define z 4)

;======================================04=======================================
;you will need to have solved problem 3. The values x,y,z are not parameters
;of this function!
(define (p4)
  (cond
    [(and (= x y) (= y z)) 0]
    [(or (and (> x z) (> z y)) (and (> z x) (> x y))) (+ x z)]
    [(or (and (> x y) (> y z)) (and (> y x) (> x z))) (+ x y)]
    [(or (and (> y z) (> z x)) (and (> z y) (> y x))) (+ z y)]
    )
)

;======================================05=======================================
(define (p5)
  (cond
    [(and (= x y) (= y z)) 0]
    [(or (and (< x y) (< y z)) (and (< y x) (< x z))) (+ x y)]
    [(or (and (< x z) (< z y)) (and (< z x) (< x y))) (+ x z)]
    [(or (and (< y z) (< z x)) (and (< z y) (< y x))) (+ y z)]
    )
)

;======================================06=======================================
(define (p6)
  (equal? x y) 
)

;======================================07=======================================
;same instructions as problem 02.
(define p7
  "The difference is that (define thirty-five 35) creates a variable named
thirty-five while (define (thirty-five) 35) calls a procedure named thirty-five
and passes the value 35 to it."
)

;======================================08=======================================
;same instructions as problem 02.
(define p8
  " This produces a literal so instead referencing a variable that was declared as
name it will instead will print a literal. "
)

;======================================09=======================================
;same instructions as problem 02.
(define p9
  "The difference is that list will actually create a datay type the holds the
elements that are given to it. While the ' creates a literal representation of what is
given to it."
)

;======================================10=======================================
;same instructions as problem 02.
(define p10
  "You can append a string while you cannot do so with a symbol. The difference is there
since while two strings can have the same contents but not be the same objects. Thus, not being
equal. On the other hand two symbols that have the same contents are guaranteed to be equal."
)

;======================================11=======================================
;(4 2 6 9)
(define (p11-1)
  (list 4 2 6 9)  
)

;(spaceship
;  (name(serenity))
;  (class(firefly)))
(define (p11-2)
  `(spaceship(name(serenity))(class(firefly)))  
)

;(2 * ((20 - (91 / 7)) * (45 - 42)))
(define (p11-3)
  `(2 * ((20 - (91 / 7)) * (45 - 42)))  
)

;======================================12=======================================
(define example '(a b c))

;(d a b c)
(define (p12-1 lst)
  (cons `d (cons (car lst) (cdr lst)))
)

;(a b d a b)
(define (p12-2 lst)
  (append (reverse (cdr (reverse lst))) (list `d) (reverse (cdr (reverse lst))))
)

;(b c d a)
(define (p12-3 lst)
  (append (cdr lst) (list `d (car lst)))
)


;======================================13=======================================
(define p13
  "Per documentation, eq? will return #t if v1 and v2 refer to the same object in memory.
While equal? is a superset of eq? which checks for if list and other such data types
have the same elements."
)
; write your answer as a string; you do not need to write any special escape
; characters to distinguish new lines.


;======================================14=======================================
(define (create-error-msg sym val)
   (string-append "This is a custom error message we will be using next. Symbol '"
                 (symbol->string sym) " was not paired with value " (number->string val)
                 )
)
;======================================15=======================================
(define (check-correctness pair)
    (cond
    [(and (and (string=? "answer-to-everything" (symbol->string (first pair))) (symbol? (first pair))) (= 42 (second pair))) #t]
    [(and (and (not (string=? "answer-to-everything" (symbol->string (first pair)))) (symbol? (first pair))) (= 42 (second pair))) #f]
    [(and (and (not (string=? "answer-to-everything" (symbol->string (first pair)))) (symbol? (first pair))) (not (= 42 (second pair)))) #f]
    [(and (and (string=? "answer-to-everything" (symbol->string (first pair))) (symbol? (first pair))) (not (= 42 (second pair)))) (raise (create-error-msg (first pair) 42))]
    )
)

;======================================16=======================================
;No answer necessary - just experiment it as instructed in hw01.txt file

