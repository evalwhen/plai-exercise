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

(define-type Value
  [numV (n : number)]
  [closV (f : (Value (Value -> Value) -> Value))])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  )

(define (interp/k [expr : ExprC] [env : Env] [k : (Value -> Value)]) : Value
  (type-case ExprC expr
    [numC (n) (k (numV n))]
    [idC (n) (k (lookup n env))]
    [plusC (l r) (interp/k l env
                           (lambda (lv)
                             (interp/k r env
                                       (lambda (rv)
                                         (k (num+ lv rv))))))]
    [multC (l r) (interp/k l env
                           (lambda (lv)
                             (interp/k r env
                                       (lambda (rv)
                                         (k (num* lv rv))))))]

    [appC (f a) (interp/k f env
                          (lambda (fv)
                            (interp/k a env
                                      (lambda (av)
                                        ((closV-f fv) av k)))))]

    [lamC (a b) (k (closV (lambda (arg-val dyn-k)
                            (interp/k b
                                      (extend-env (bind a arg-val)
                                                  env)
                                      dyn-k))))]))


(define (interp [expr : ExprC]) : Value
  (interp/k expr mt-env
            (lambda (ans)
              ans)))


(test (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10))))
      (numV 15))


;(define (f1 x)
;  (f2 4))
;(define (f2 y)
;  (+ x y))
;(f1 2)
; test lexical scope
(test (interp (appC (lamC 'x (appC (lamC 'y (plusC (idC 'x) (idC 'y)))
                                   (numC 4)))
                    (numC 3)))
      (numV 7))

(define-syntax (generator e)
  (syntax-case e ()
    [(generator (yield) (v) b)
     #'(let ([where-to-go (lambda (v) (error 'where-to-go "nothing"))])
         (letrec ([resumer (lambda (v)
                             (begin b
                                    (error 'generator "fell through")))]
                  [yield (lambda (v)
                           (let/cc gen-k
                             (begin
                               (set! resumer gen-k)
                               (where-to-go v))))])
           (lambda (v)
             (let/cc dyn-k
               (begin
                 (set! where-to-go dyn-k)
                 (resumer v))))))]))

(define g1 (generator (yield) (v)
                      (letrec ([loop (lambda (n)
                                       (begin
                                         (yield n)
                                         (loop (+ n 1))))])
                        (loop v))))
