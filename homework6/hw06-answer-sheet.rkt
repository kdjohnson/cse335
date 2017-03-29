#lang racket
(#%provide (all-defined))
(#%require (lib "eopl.ss" "eopl"))
(#%require "hw06-env-values.rkt")

;===============================================================================
;========================= Lexical and Grammar Specs ===========================
;===============================================================================

(define lexical-spec
  '(
    (whitespace (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    )
  )

;write your answer in string form:
(define problem-1-answer
  "The lexcial-spec defines that comments are denoted by the # symbol
   and that there can be an arbitray amount of them and they can continue
   until the end of the line."
  )

#|
<expr> ::=
           number
        | up(<expr>) "up-expr"
        | down(<expr>) "down-expr"
        | left(<expr>)  "left-expr"
        | right(<expr>)  "right-expr"
        | (<expr> <expr>)  "point-expr"
        | + <expr> <expr>  "add-expr"
        | origin? (<expr>)  "origin-expr"
        | if (<expr>)
           then <expr>
           else <expr>       "if-expr"
        | move (<expr> <expr> <expr>*)  "move-expr"
|#


#|
(block-expr
  (block-expr19 (list-of var-expr?))
  (block-expr20 (list-of expr?))
)

(var-expr
  (var (var21 symbol?) (var22 expr?)))

(expr ("{" (abrno var-expr) (arbno expr) "}") block-expr)
(var-expr ("val" identifier "=" expr) var)
|#
(define grammar-spec
  '(
    ;please remove this before you start writting your gramar
    ;(unimplemented ("unimlpemented") unimplemented-expr)
    (program (expr (arbno expr)) a-program)
    (expr (number) num-expr)
    (expr ("up" "(" expr ")") up)
    (expr ("down" "(" expr ")") down)
    (expr ("left" "(" expr ")") left)
    (expr ("right" "(" expr ")") right)
    (expr ("(" expr expr ")") point-expr)
    (expr ("+" expr expr) add-expr)
    (expr ("origin? (" expr ")") origin-expr)
    (expr ("if" expr " then " expr " else " expr) if-expr)
    (expr ("move(" expr expr (arbno expr)")") move-expr)
    (expr ("{" (arbno var-expr) (arbno expr) "}") block-expr)
    (expr (identifier) v-expr)
    (var-expr ("val" identifier "=" expr) var)
    (var-expr ("final val" identifier "=" expr) final-val)
    )
  )

;given one or more arguments this function will return a flat list
(define (flat-list el1 . rest)
  (flatten (list el1 rest))
  )
;===============================================================================
;================================ Value-of =====================================
;===============================================================================
;value-of takes as a parameter an AST resulted from a call to the
;parser function.
(define (run program-string)
  program-string
  (if (string? program-string)
      (value-of (parser program-string) (empty-env))
      (raise ("value-of-ast-node-type error: unimplemented expression: " (~a program-string)))
      )
  )

(define (value-of ast env)
  (cond
    [(program? ast) (value-of-program ast env)]
    [(expr? ast) (value-of-expr ast env)]
    )
  )

(define (value-of-program ast env)
  (cases program ast
    (a-program (expr remaining)
               (andmap
                (lambda (ex) (value-of ex env))
                (flat-list expr remaining)))
    )
  )

#|
(define (value-of-var var env)
  (if (var-expr? var)
      (cases var-expr var
        (val (identify val-identify) (extend-env-wrapper identify (value-of val-identify env) env NON-FINAL))
        (final-val (identify val-identify) (extend-env-wrapper identify (value-of val-identify env) env FINAL))
        (else (raise (~a "value-of-var error: " var)))
        )
      (invalid-args-exception "value-of-var" "var-expr?" var)
      )
  )
|#
;(left-expr     (num) (step-val (left-step (num-val->n (value-of num env)))))



(define (value-of-expr ast env)
  (cases expr ast
    (num-expr (n) (num-val n))
    (up (n) (step-val (up-step (num-val->n (value-of n env)))))
    (down (n) (step-val (down-step (num-val->n (value-of n env)))))
    (left (n) (step-val (left-step (num-val->n (value-of n env)))))
    (right (n) (step-val (right-step (num-val->n (value-of n env)))))
    (point-expr (pt1 pt2) (point-val (point (num-val->n (value-of pt1 env)) (num-val->n (value-of pt2 env)))))
    (add-expr (left right) (add-helper left right))
    (origin-expr (ex) ex)
    (if-expr (ex1 ex2 ex3) "!!!!")
    (move-expr (ex1 ex2 ex3) "nwoeijowej")
    (block-expr (var expr) "block expr")
    (v-expr (i) i)
    (else "fuck")
    )
  )

(define add-helper
  (lambda (left right)
    (letrec
        ([left-expr (step-val->st (value-of left (empty-env)))]
         [right-expr (step-val->st (value-of right (empty-env)))]
         )
      (if (follows-add-rules? left-expr right-expr)
          (cond
            [(or (up-step? left-expr) (down-step? left-expr)) (vertical-axis (+ (convert-to-cartesian left-expr) (convert-to-cartesian right-expr)))]
            [(or (left-step? right-expr) (right-step? right-expr)) (horizontal-axis (+ (convert-to-cartesian left-expr) (convert-to-cartesian right-expr)))]
            [else (raise (invalid-args-exception "add-helper" "add-expr" (~a left right)))]
            )
          (raise (invalid-args-exception "add-helper" "add-expr" (~a left right)))
          )
      )
    )
  )

(define vertical-axis
  (lambda (y)
    (if (positive? y)
        (step-val (up-step y))
        (step-val (down-step (* -1 y)))
        )
    )
  )

(define horizontal-axis
  (lambda (x)
    (if (positive? x)
        (step-val (right-step x))
        (step-val (left-step (* -1 x)))
        )
    )
  )

(define convert-to-cartesian
  (lambda (st)
    (cases step st
      (up-step (n) n)
      (right-step (n) n)
      (down-step (n) (* -1 n))
      (left-step (n) (* -1 n))
      )
    )
  )

(define follows-add-rules?
  (lambda (addend1 addend2)
    (or (and (left-step? addend1) (left-step? addend2))
        (and (left-step? addend1) (right-step? addend2))
        (and (right-step? addend1) (right-step? addend2))
        (and (right-step? addend1) (left-step? addend2))
        (and (down-step? addend1) (down-step? addend2))
        (and (down-step? addend1) (up-step? addend2))
        (and (up-step? addend1) (down-step? addend2))
        (and (up-step? addend1) (up-step? addend2))
        )
    )
  )


;for each different ast node type, e.g. <program>, <expr>, <var-expr> you might
;consider implementing a function with the outline:
#|
(define (value-of-ast-node-type ast)
  (cases ast-node-type ast
    (ast-node-type-variant
     (f1 f2)
     'UNIMPLEMENTED
     )
    (else (raise (~a "value-of-ast-node-type error: unimplemented expression: " ast)))
    )
  )
|#
;===============================================================================
;============================= sllgen boilerplate ==============================
;===============================================================================
;this will create the AST datatype with define-datatype
;according to the lexical and grammar specifications.
(sllgen:make-define-datatypes lexical-spec grammar-spec)

;you can use this function to display the define-datatype
;expression used to generate the AST. Take some time to read it.
;you should be able to understand it by now.
(define (show-data-types)
  (sllgen:list-define-datatypes lexical-spec grammar-spec))

;parser is a one argument function that takes a string,
;scans & parses it and generates a resulting abstract
;syntax tree (ast). 
(define parser
  (sllgen:make-string-parser lexical-spec grammar-spec))

;you can use this function to find out more about how
;the string is broken up into tokens during parsing,
;this step is automatically included in the create-ast
;function. This is a one-argument function that takes a 
;string.
(define scanner
  (sllgen:make-string-scanner lexical-spec grammar-spec))