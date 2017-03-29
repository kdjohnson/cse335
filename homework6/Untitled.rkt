#lang racket
(#%require (lib "eopl.ss" "eopl"))

(define lexical-spec
  '(
    (whitespace (whitespace) skip)
    (comments (";" (arbno (not #\newline))) skip)
    (num (digit (arbno digit)) number)
    )
  )

(define grammar-spec
  '(
    (program (step) a-program)
    (step ("left" num) left-step)
    (step ("right" num) right-step)
    (step ("(" step step ")") seq-step) 
    )
  )

(sllgen:make-define-datatypes lexical-spec grammar-spec)

(define (show-data-types)
  (sllgen:list-define-datatypes lexical-spec grammar-spec)
  )
(define parser
  (sllgen:make-string-parser lexical-spec grammar-spec)
  )
(define scanner
  (sllgen:make-string-scanner lexical-spec grammar-spec)
  )
