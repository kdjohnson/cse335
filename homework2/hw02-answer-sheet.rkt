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
     for the problem are still present. 
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. 
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!
|#
;======================================01=======================================
(define (list-of-even-numbers? lst)
  (cond
    [(null? lst) #t]
    [(not (pair? lst)) #f]
    [(not (number? (car lst))) #f]
    [(or (symbol? (car lst)) (string? (car lst))) #f]
    [(even? (car lst)) (list-of-even-numbers? (cdr lst))]
    [(odd? (car lst)) #f]
    )  
  )

;======================================02=======================================
;;for n > 0
;Sn = 1/1 + 1/4 + 1/9 + 1/16 + ...
(define (series-a n)
  (if (= n 0)
      0
      (+ (/ 1 (expt n 2)) (series-a (- n 1)))
      )
  )

;====
;;for n >= 0
;Sn = 1 - 1/2 + 1/6 - 1/24 + ...
(define (series-b n)
  (if (= n 0)
      1
      (+ (/ (pow n) (fact-series-b n)) (series-b( - n 1)))
      )
  )

(define (pow n)
  (if (= n 0)
      1
      (expt -1 n)
      )
  )

(define (fact-series-b n)
  (fact-tail (+ n 1) 1)
  )

(define (fact-tail n prod)
  (if (= n 0)
      prod
      (fact-tail (- n 1) (* n prod))
      )
  )

;======================================03=======================================
(define (carpet n)
  (if (= n 0)
      '((%))
      (add-top-bom (expand-each-line (carpet (- n 1)) (decide-sym n)) n (decide-sym n))
      )  
  )

(define (decide-sym n)
  (if (odd? n)
      '+
      '%))

(define (expand-each-line carp sym) ;sym=> %, or +
  (if (null? carp)
      '()
      (cons (append  (cons sym (car carp) ) (list sym)) ; expanded first line
            (expand-each-line (cdr carp) sym)))  ; expanded the rest of the carpet except for the first line
  )

(define (add-top-bom carp n sym)
  (append (cons (compose-top n sym) carp)
          (list (compose-top n sym))))

(define (compose-top n sym)
  (map (lambda (x) sym) (range (+ 1 (* 2 n))))
  )


;======================================04=======================================
(define (pascal n)
  (if (= n 1)
      `((1))
      (append (pascal (- n 1)) (list (append (map (lambda (num) (combinations (- n 1) num)) (range (- n 1))) (list 1))))
      ) 
  )

(define (combinations n k)
  (/ (fact n) (* (fact k) (fact (- n k))))
  )

(define (fact n)
  (fact-tail n 1)
  )

;======================================05=======================================
(define (balanced? in)
  (if (< (check-string (string->list in)) 0)
      #f
      #t
      ) 
  )

(define (check-string lst)
  (define counter 0)
  (if (null? lst)
      (- counter 1)
      (if (and (eqv? (car lst) #\() (eqv? (last lst) #\)))
          (+ counter 1)
          (check-string (reverse (cdr (reverse (stop (cdr lst))))))
          )
      )
  )

(define (stop lst)
  (if (null? lst)
      `(1)
      lst)
  )

;======================================06=======================================
(define (list-of-all? predicate lst)
    (if (null? lst)
      #t
      (and (list-of-all? predicate (cdr lst)) (predicate (car lst)))
      ) 
  )

;======================================07=======================================
(define (create-mapping keys vals)
  'UNIMPLEMENTED  
  )