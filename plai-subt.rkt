#lang plai-typed

;(define-type ArithC
;  [numC (n : number)]
;  [plusC (l : ArithC) (r : ArithC)]
;  [multC (l : ArithC) (r : ArithC)])

;; type for surface syntax
(define-type ExprS
  [numS (n : number)]
  [idS (id : symbol)]
  [appS (f : symbol) (a : ExprS)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)])

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS (n) (numC n)]
    [idS (id) (idC id)]
    [appS (f a) (appC f (desugar a))]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
;    [bminusS (l r) (plusC (desugar l)
;                          (multC (numC -1) (desugar r)))]
    [bminusS (l r) (fault-tolerant (desugar l) (desugar r))]
    [uminusS (e) (fault-tolerant (numC 0) (desugar e))]))

(define (fault-tolerant [l : ExprC] [r : ExprC]) : ExprC
  (plusC l (multC (numC -1) r)))
                        
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (if (= (length sl) 2)
                  (uminusS (parse (second sl)))
                  (bminusS (parse (second sl)) (parse (third sl))))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define (get-fundef [fname : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    ((cons? fds) (cond
                   [(equal? fname (fdC-name (first fds))) (first fds)]
                   [else (get-fundef fname (rest fds))]))))

(define (subst [what : number] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) (numC what)]
               [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]))
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define (interp [a : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
    [numC (n) n]
    [idC (_) (error 'interp "shouldn't get here")]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst (interp a fds)
                                 (fdC-arg fd)
                                 (fdC-body fd))
                          fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]))

(define empty-fds (list))
(define (r3 [s : s-expression]) : number
  (interp (desugar (parse s)) empty-fds))

(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              (list (fdC 'const5 '_ (numC 5))))
      15)
 