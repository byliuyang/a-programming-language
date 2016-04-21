#lang plai-typed

;; starter file for the extended basic interpreter assignment
; want static scope

;---------------------------------------------------------------------------------
;; surface syntax and parser : you should NOT need to edit this section

; type used to capture a with-binding
(define-type DefS
  [defS (name : symbol) (val : ExprS)])

; surface syntax for expressions
(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [idS (i : symbol)]
  [appS (f : ExprS) (args : (listof ExprS))]
  [if0S (c : ExprS) (t : ExprS) (e : ExprS)]
  [funS (params : (listof symbol)) (body : ExprS)]
  [withS (bindings : (listof DefS)) (body : ExprS)]
  )

; parses s-expressions into surface syntax
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond [(s-exp-symbol? (first sl)) ;; built-in construct or calling function through an identifier
              (case (s-exp->symbol (first sl))
                [(+) (plusS (parse (second sl)) (parse (third sl)))]
                [(*) (multS (parse (second sl)) (parse (third sl)))]
                [(-) (bminusS (parse (second sl)) (parse (third sl)))]
                [(if0) (if0S (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [(fun) (funS (map s-exp->symbol (s-exp->list (second sl))) 
                                (parse (third sl)))]
                [(with) (withS (map (lambda (b) 
                                      (let ([bl (s-exp->list b)])
                                        (defS (s-exp->symbol (first bl)) (parse (second bl)))))
                                    (s-exp->list (second sl)))
                               (parse (third sl)))]
                [else ;; must be a function call using function name
                 (appS (idS (s-exp->symbol (first sl)))
                       (map parse (rest sl)))])]
             [(s-exp-list? (first sl)) ;; function call with complex expression in function position
              (appS (parse (first sl))
                    (map parse (rest sl)))]
             [else (error 'parse "expected symbol or list after parenthesis")]))]
    [else (error 'parse "unexpected input format")]))
     
;---------------------------------------------------------------------------------
;; abstract syntax and desugar
     
(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (i : symbol)]
  [appC (f : ExprC) (arg : (listof ExprC))]
  [if0C (c : ExprC) (t : ExprC) (e : ExprC)]
  [funC (params : (listof symbol)) (body : ExprC)])

;; desugar -- returning a default/dummy value so file can be run
(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [numS (n) (numC n)]
    [plusS (l r) (plusC
                  (desugar l)
                  (desugar r))]
    [bminusS (l r) (plusC
                    (desugar l)
                    (multC
                     (numC -1)
                     (desugar r)))]
    [multS (l r) (multC
                  (desugar l)
                  (desugar r))]
    [idS (i) (idC i)]
    [appS (f a) (appC
                 (desugar f)
                 (map
                  (lambda (arg) (desugar arg))
                  a))]
    [if0S (c t e) (if0C (desugar c) (desugar t) (desugar e))]
    [funS (p b) (funC p (desugar b))]
    [withS (bindings body) (appC
                            (funC
                             (map (lambda (b) (defS-name b)) bindings) (desugar body))
                            (map (lambda (b) (desugar (defS-val b))) bindings))]))

;---------------------------------------------------------------------------------
;; output values

(define-type Value
  [numV (n : number)]
  [closV (args : (listof symbol)) (body : ExprC) (env : Env)])

;---------------------------------------------------------------------------------
;; Environments

;; binding an identifier to a value
(define-type Binding
  [bind (name : symbol) (val : Value)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env append)
; Store
(define-type-alias Location number)
(define-type Store
  [cell (loc : Location) (val : Value)])

; Result
(define-type Result
  [v*s (v : Value) (s : Store)])

; Bind values to parameters
(define (bind-vals [params : (listof symbol)] [vals : (listof Value)]) : Env
      (cond
        [(empty? params) empty] ; all expressions are binded to parameters
        [else
         (if (member 
              (first params)
              (rest params)) ; Check whether is repeated
             (error 'bind-vals "multiple")
             (cons
              (bind
               (first params)
               (first vals)) ; bind parameter to value
              (bind-vals ; bind the rest of params to values
               (rest params)
               (rest vals))))]))

; Number addition
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "at least one argument was not a number")]))

; Number multiplication
(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "at least one argument was not a number")]))

; lookup symbol from environment
(define (lookup (for : symbol) (env : Env)) : Value
  (cond
    [(empty? env) (error 'lookup "unbound")]
    [else (cond
       [(symbol=? for (bind-name (first env))) (bind-val (first env))]
       [else (lookup for (rest env))])]))

; Binary operator
(define (binary-opt ([operator : ][l : ExprC] [r : ExprC]))

;---------------------------------------------------------------------------------
;; interp -- returning a default/dummy value so file can be run
(define (interp [e : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC e
    [numC (n) (numV n)] ; number
    [plusC (l r) (num+ (interp l env sto) (interp r env sto))] ; plus
    [multC (l r) (num* (interp l env sto) (interp r env sto))] ; multiply
    [idC (i) (lookup i env)] ; look up value for id
    [appC (f a)
          (let
              ([fd (interp f env)]) ; Interprete the function 
            (interp
             (closV-body fd) ; Interprete function body
             (extend-env
              (bind-vals ; binding arguments to parameters
               (closV-args fd) 
               (map (lambda (expr) (interp expr env)) a)) ; Eager semantics
              (extend-env ; static scope
               (closV-env fd)
               env))))]
    [if0C (c t e) (let ([n (interp c env)])
                    (cond
                      [(numV? n) ; condition is number
                       (cond
                         [(= (numV-n n) 0) ; condition is 0
                          (interp t env)] ; Then
                         [else ; Else
                          (interp e env)])]
                      [else (error 'interp "type") ; Not number
                            ]))]
    [funC (p b)
          (closV p b env) ; static scope
          ]))

;---------------------------------------------------------------------------------
;; API for running programs

; evaluates a program starting with a pre-populated environment
; (this can be helpful in testing)
(define (run/env sexp env)
  (interp (desugar (parse sexp)) env))

; evaluates a program in the empty environment
(define (run sexp)
  (run/env sexp mt-env))

(run '(with ([b (box 5)])
            (+ (seq (setbox b 8)
                    (unbox b))
               (unbox b))))