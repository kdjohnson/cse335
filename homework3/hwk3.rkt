#lang racket
(require racket/trace)

(define (foldl-335 op default-el lst)
  (if (null? lst)
      default-el
      (op (foldl-335 op default-el (cdr lst)) (car lst))
      )
  )


(define (foldr-335 op default-el lst)
  (if (null? lst)
      default-el
      (op (car lst) (foldr-335 op default-el (cdr lst)))
      )
  )

(define (andmap-335 test-op lst)
  (if (null? lst)
      #t
      (and (test-op (car lst)) (andmap-335 test-op (cdr lst)))
      )
  )

(define (filter-335 test-op lst)
  (remove* (list '()) (map (lambda (x) (if (test-op x) x '())) lst))
  )


(define add-forty-two
  (lambda (x) (+ x 42)))

(define (map-reduce m-op r-op default-el lst)
  (foldl r-op default-el (map m-op lst))
  )

(define (fact n)
  (fact-tail n 1)
  )

(define (fact-tail n prod)
  (if (= n 0)
      prod
      (fact-tail (- n 1) (* n prod))
      )
  )

(define (series n)
  (foldl + 0 (map (lambda (x) (/ (expt -1 x) (fact-tail (+ 1 x) 1))) (range 0 (+ n 1))))
  )

(define (zip lst1 lst2)
  (map list lst1 lst2)
  )

(define n-matrix 
  '((1 2 3 4)
    (5 6 7 8)
    (9 0 1 2))
  )

(define str-matrix
  '(("a" "c" "e")
    ("b" "d" "f"))
  )

(define (zipper mat)
  (append (list (map (lambda (x) (car x)) mat)) (list (map (lambda (x) (cadr x)) mat))
          (list (map (lambda (x) (caddr x)) mat)) (list (map (lambda (x) (cadddr x)) mat)))
  )


#|
(define (tst mat)
  (if (null? mat)
      '()
      (tst (filter pair? (map cdr mat)))
      )
  )

|#

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