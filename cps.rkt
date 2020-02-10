#lang plai-typed

(define (read-number [prompt : string]) : number
  (begin
    (display prompt)
    (let ([v (read)])
      (if (s-exp-number? v)
          (s-exp->number v)
          (read-number prompt)))))
;
;(display
;  (+ (read-number "First number")
;     (read-number "Second number")))

(define-type-alias label number)

(define new-label
  (let ([counter 0])
    (lambda ()
      (begin (set! counter (+ counter 1))
             counter))))

(define table (make-hash empty))

(define (read-number/suspend [prompt : string] rest)
  (let ([g (new-label)])
    (begin
      (hash-set! table g rest)
      (display prompt)
      (display " To enter it, use the action field label ")
      (display g)
      (display "\n")
      ((error 'halting "Program shut down")))))

(define (resume [g : label] [n : number])
  ((some-v (hash-ref table g)) n))

;(read-number/suspend "First number"
;                     (lambda (v1)
;                       (display
;                        (+ v1
;                           (read-number "Second number")))))

;(read-number/suspend "First number"
;                     (lambda (v1)
;                       (read-number/suspend "Second number"
;                                            (lambda (v2)
;                                              (display
;                                               (+ v1 v2))))))

(define cookie '-100)
 
(read-number/suspend "First number"
                     (lambda (v1)
                       (begin
                         (set! cookie v1)
                         (read-number/suspend "Second number"
                                            (lambda (v2)
                                              (display
                                               (+ cookie v2)))))))


;(read-number/stateless "First number" prog1)
; 
;(define (prog1 v1)
;  (read-number/stateless "Second number" prog2))
; 
;(define (prog2 v2)
;  (display (+ v1 v2)))


