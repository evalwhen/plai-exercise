#lang plai-typed

;(define-type ArithC
;  [numC (n : number)]
;  [plusC (l : ArithC) (r : ArithC)]
;  [multC (l : ArithC) (r : ArithC)])

;; type for surface syntax

;; language core==============================================
(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

;; This is an instance of a safe run-time system
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "one argument was not a number")]))

;(define-type FunDefC
;  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])

(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [appC (f a) (cond
                  [(lamC? f)
                   (local ([define fd (interp f env)])
                     (interp (closV-body fd)
                             (extend-env (bind (closV-arg fd)
                                               (interp a env))
                                         (closV-env fd))))]
                  [else (error 'appC "f is not a function value")])]
    ;    [appC (f a) (interp (fdC-body f)
    ;                          (extend-env (bind (fdC-arg f)
    ;                                            (interp a env))
    ;                                      mt-env))]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [lamC (a b) (closV a b env)]))

(test (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))

(test/exn (interp (plusC (numC 10) (appC (numC 1) (numC 10)))
              mt-env)
      "f is not a function value")

;(define (f1 x)
;  (f2 4))
;(define (f2 y)
;  (+ x y))
;(f1 2)
; test lexical scope
(test (interp (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3))
                  mt-env)
          (numV 7))