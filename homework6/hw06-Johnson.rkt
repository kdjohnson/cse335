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
   until the end of the line. It also denotes that whitespace will be skipped
   and not interpreted at all."
  )

(define grammar-spec
  '(
    (program (expr (arbno expr)) a-program)
    (expr (number) num-expr)
    (expr ("up" "(" expr ")") up-expr)
    (expr ("down" "(" expr ")") down-expr)
    (expr ("left" "(" expr ")") left-expr)
    (expr ("right" "(" expr ")") right-expr)
    (expr ("(" expr expr ")") point-expr)
    (expr ("+" expr expr) add-expr)
    (expr ("origin?" "(" expr ")") origin-expr)
    (expr ("if" "(" expr ")" "then" expr "else" expr) if-expr)
    (expr ("move" "(" expr expr (arbno expr) ")") move-expr)
    (expr ("{" (arbno var-expr) (arbno expr) "}") block-expr)
    (expr (identifier) iden-expr)
    (var-expr ("val" identifier "=" expr) val)
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
  (cond
    [(string? program-string) (value-of (parser program-string) (empty-env))]
    [else (raise (string-append "value-of-ast-node-type error: unimplemented expression: " (~a program-string)))]
    )
  )

(define (value-of ast env)
  (cond
    [(program? ast) (value-of-program ast env)]
    [(expr? ast) (value-of-expr ast env)]
    [(var-expr? ast) (value-of-var ast env)]
    [else (raise "value-of")]
    )
  )

(define (value-of-program ast env)
  (cases program ast
    (a-program (expr remaining)
               (andmap
                (lambda (ex) (value-of ex env))
                (flat-list expr remaining)))
    (else (raise "value-of-program"))
    )
  )            


(define (value-of-expr ast env)
  (cases expr ast
    (num-expr (n) (num-val n))
    (up-expr (n) (step-val (up-step (num-val->n (value-of n env)))))
    (down-expr (n) (step-val (down-step (num-val->n (value-of n env)))))
    (left-expr (n) (step-val (left-step (num-val->n (value-of n env)))))
    (right-expr (n) (step-val (right-step (num-val->n (value-of n env)))))
    (point-expr (pt1 pt2) (point-val (point (num-val->n (value-of pt1 env)) (num-val->n (value-of pt2 env)))))
    (add-expr (left right) (add-helper left right))
    (origin-expr (ex) (bool-val (origin-helper (value-of ex '()))))
    (if-expr (predicate return1 return2) (if-helper predicate return1 return2))
    (move-expr (pt st1 st2) (move-helper pt st1 st2))
    (block-expr (lst1 lst2) (andmap (lambda (x) (value-of x (build-new-env lst1 env))) lst2))
    (iden-expr (x) (apply-env env x))
    (else "value-of-expr")
    )
  )

(define (value-of-var ast env)
  (cases var-expr ast
    (val (identifier var-expr) (extend-env-wrapper identifier (value-of var-expr env) env NON-FINAL))
    (final-val (identifier var-expr) (extend-env-wrapper identifier (value-of var-expr env) env FINAL))
    (else (raise "value-of-var"))
    )
  )

(define (build-new-env lst1 old-env)
  (if (null? lst1)
      old-env
      (build-new-env (cdr lst1) (add-one-at-a-time (car lst1) old-env))))

(define add-one-at-a-time
    (lambda (lst1 old-env)
      (cases var-expr lst1
        (val (iden expr) (extend-env-wrapper iden (value-of expr old-env) old-env NON-FINAL))
        (final-val (iden expr) (extend-env-wrapper iden (value-of expr old-env) old-env FINAL))
        )
      )
  )

(define move-fold-helper
  (lambda (st pnt)
    (cases step st
      (up-step (n) (point (point->x pnt) (+ n (point->y pnt))))
      (down-step (n) (point (point->x pnt) (+ (* -1 n) (point->y pnt))))
      (left-step (n) (point (+ (* -1 n) (point->x pnt)) (point->y pnt)))
      (right-step (n) (point (+ n (point->x pnt)) (point->y pnt)))
      (else (raise "move-fold-helper"))
      )
    )
  )


(define move-helper
  (lambda (pt step1 remain-steps)
    (letrec
        ([pnt (point-val->p (value-of pt '()))]
         [st (step-val->st (value-of step1 '()))]
         [list-expr (flat-list step1 remain-steps)]
         [all (map (lambda (ex) (step-val->st (value-of ex '()))) list-expr)]
         [steps (foldl move-fold-helper pnt all)]
         )
      (point-val steps)
      ) 
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

(define origin-helper
  (lambda (expr)
    (letrec
        ([point (point-val->p expr)])
      (if (and (= 0 (point->x point)) (= 0 (point->y point)))
          #t
          #f
          )
      )
    )
  )

(define if-helper
  (lambda (ex r1 r2)
    (let
        [(cond (bool-val->b (value-of ex '())))]
      (if cond
          (value-of r1 '())
          (value-of r2 '())
          )
      )
    )
  )

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