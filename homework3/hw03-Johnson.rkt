#lang racket
(#%provide (all-defined))

#|
IMPORTANT:
Overall, you are allowed to change this file in any way that does *not* affect the
compilation of the accompanying test file. Changes that are almost certain to break
the above restriction are:
  - changing the names of any definitions that are explicitely used in the tests
    (e.g. function names, relevant constants)

If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:

   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     because changing the number of arguments automatically changes the semantics of the 
     function. Changing the name of the arguments is permitted since that change only
     affects the readability of the function, not the semantics.
   - you may write any number of helper functions as you want.

When done, make sure that the accompanying test file compiles. 
|#
;======================================01=======================================
(define (foldl-335 op default-el lst)
  (if (null? lst)
      default-el
      (op (foldl-335 op default-el (cdr lst)) (car lst))
      )  
  )

;---
(define (foldr-335 op default-el lst)
  (if (null? lst)
      default-el
      (op (car lst) (foldr-335 op default-el (cdr lst)))
      )  
  )

;======================================02=======================================
(define (andmap-335 test-op lst)
  (if (null? lst)
      #t
      (and (test-op (car lst)) (andmap-335 test-op (cdr lst)))
      )
  )

;======================================03=======================================
(define (filter-335 test-op lst)
  (remove* (list '()) (map (lambda (x) (if (test-op x) x '())) lst))
  )

;======================================04=======================================
(define (map-reduce m-op r-op default-el lst)
  (foldl r-op default-el (map m-op lst))
  )

;======================================05=======================================
(define (fact-tail n prod)
  (if (= n 0)
      prod
      (fact-tail (- n 1) (* n prod))
      )
  )

(define (series n)
  (foldl + 0 (map (lambda (x) (/ (expt -1 x) (fact-tail (+ 1 x) 1))) (range 0 (+ n 1))))  
  )

;======================================06=======================================
(define (zip lst1 lst2)
  (map list lst1 lst2)  
  )

;======================================07=======================================
(define (zippy mat)
  (if (null? mat)
      '()
      (append (list (map (lambda (x) (car x)) mat)) (zippy (filter pair? (map cdr mat))))
      )
  )

(define (matrix-to-vector op mat)
  (if (number? (caar mat)) 
      (map (lambda (x) (foldr op 0 x)) (zippy mat))
      (map (lambda (x) (foldr op "" x)) (zippy mat))
      )  
  )

