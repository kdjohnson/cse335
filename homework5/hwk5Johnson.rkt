#lang racket
(#%require (lib "eopl.ss" "eopl"))

(define-datatype step step?
  (left-step (num number?))
  (right-step (num number?))
  (up-step (num number?))
  (down-step (num number?))
  (seq-step (st1 step?) (st2 step?))
  )

(define left-step?
  (lambda (st)
    (and (step? st)
         (cases step st
           (left-step (n) #t)
           (else #f)
           )
         )
    )
  )

(define right-step?
  (lambda (st)
    (and (step? st)
         (cases step st
           (right-step (n) #t)
           (else #f)
           )
         )
    )
  )

(define up-step?
  (lambda (st)
    (and (step? st)
         (cases step st
           (up-step (n) #t)
           (else #f)
           )
         )
    )
  )

(define (invalid-args-msg func expected received)
  (string-append  "Inavalid arguments in: " func " --- expected: " expected " --- received: "  received)
  )

(define down-step?
  (lambda (st)
    (and (step? st)
         (cases step st
           (down-step (n) #t)
           (else #f)
           )
         )
    )
  )

(define seq-step?
  (lambda (st)
    (and (step? st)
         (cases step st
           (seq-step (st1 st2) #t)
           (else #f)
           )
         )
    )
  )

(define single-step?
  (lambda (st)
    (and (step? st)
         (cases step st
           (seq-step (st1 st2) #f)
           (else #f)
           )
         )
    )
  )

(define seq-step->st-1
  (lambda (st)
    (if (seq-step? st)
        (cases step st
          (seq-step (st1 st2) st1)
          (else (invalid-args-msg "single-step->st-1" "seq-step?" st))
          )
        (raise (invalid-args-msg "single-step->st-1" "seq-step?" st))
        )
    )
  )

(define seq-step->st-2
  (lambda (st)
    (if (seq-step? st)
        (cases step st
          (seq-step (st1 st2) st2)
          (else (invalid-args-msg "single-step->st-2" "seq-step?" st))
          )
        (raise (invalid-args-msg "single-step->st-2" "seq-step?" st))
        )
    )
  )

(define single-step->n
  (lambda (st)
    (if (step? st)     
        (cases step st
          (up-step (n) n)
          (left-step (n) n)
          (down-step (n) n)
          (right-step (n) n)
          (else (invalid-args-msg "single-step->n" "single-step?" st))
          )
        (raise (invalid-args-msg "single-step->n" "single-step?" st))
        )
    )
  )

(define move
  (lambda (pt st)
    (cases step st
      (up-step (n) (list (car pt) (+ (cadr pt) (single-step->n st))))
      (down-step (n) (list (car pt) (- (cadr pt) (single-step->n st))))
      (left-step (n) (list (- (car pt) (single-step->n st)) (cadr pt)))
      (right-step (n) (list (+ (car pt) (single-step->n st)) (cadr pt)))
      (seq-step (st1 st2) (move (move pt st2) st1))
      )
    )
  )

(define-datatype environment environment?
  (empty-env)
  (extend-env (var symbol?) (val number?) (env environment?))
  (extend-env-final (var symbol?) (val number?) (env environment?))
  )

(define (exception-no-binding-msg sym)
  (string-append "No binding for '" (~a sym))
  )

(define apply-env
  (lambda (env search-val)
    (cases environment env
      (empty-env () (exception-no-binding-msg search-val))
      (extend-env (saved-var saved-val saved-env)
                  (if (eqv? search-val saved-var)
                      saved-val
                      (apply-env saved-env search-val)
                      )
                  )
      (extend-env-final (saved-var saved-val saved-env)
                        saved-val
                        )
      )
    )
  )


(define FINAL #t)
(define NON-FINAL #f)
(define (exception-sym-final-msg sym)
  (string-append "Symbol '" (~a sym) " is final and cannot be overriden.")
  )

(define extend-env-wrapper
  (lambda (sym val old-env final?)  
    (if (is-final? old-env sym)
        (exception-sym-final-msg sym)
        (if final?
            (extend-env-final sym val old-env)
            (extend-env sym val old-env)
            )
        )
    )
  )

(define is-final?
  (lambda (env sym)
    (cases environment env
      (empty-env () #f)
      (extend-env (kept-sym kept-val prev-env) (is-final? prev-env sym))
      (extend-env-final (kept-sym kept-val prev-env) (if (equal? sym kept-sym)
                                                         #t
                                                         (is-final? prev-env sym)
                                                         ))
      )
    )
  )


;(define x-42-nf (extend-env-wrapper 'x 42 (empty-env) NON-FINAL))
;(define x-42-f (extend-env-wrapper 'x 42 (empty-env) FINAL))

(define x-42-nf (extend-env-wrapper 'x 42 (empty-env) NON-FINAL))
    (define x-42-f (extend-env-wrapper 'x 42 (empty-env) FINAL))
    
    (define y-24-nf$x-42-nf (extend-env-wrapper 'y 24 x-42-nf NON-FINAL))
    (define y-24-nf$x-42-f (extend-env-wrapper 'y 24 x-42-f NON-FINAL))