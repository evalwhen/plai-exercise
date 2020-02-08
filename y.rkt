(
 ((lambda (length)
    (lambda (ls)
      (cond
        [(empty? ls) 0]
        [else (add1 ((length length) (rest ls)))])))
  (lambda (length)
    (lambda (ls)
      (cond
        [(empty? ls) 0]
        [else (add1 ((length length) (rest ls)))]))))
 '(1 2 3)
 )

(((lambda (u) (u u))
  (lambda (length)
    (lambda (ls)
      (cond
        [(empty? ls) 0]
        [else (add1 ((length length) (rest ls)))]))))
 '(1 2 3))

;; bug version; while non-termination for (length length)
(((lambda (u) (u u))
  (lambda (length)
    ((lambda (g)
       (lambda (ls)
         (cond
           [(empty? ls) 0]
           [else (add1 (g (rest ls)))])))
     (length length))))
 '(1 2 3))

;; defer (length length)
(((lambda (u) (u u))
   (lambda (length)
     ((lambda (g)
        (lambda (ls)
          (cond
            [(empty? ls) 0]
            [else (add1 (g (rest ls)))])))
      (lambda (v) ((length length) v)))))
'(1 2 3))

;;
(lambda (f)
  ((lambda (u) (u u))
   (lambda (length)
     (f
      (lambda (v) ((length length) v))))))
;; rename
(lambda (f)
  ((lambda (u) (u u))
   (lambda (x) (f (lambda (v) ((x x) v))))))

;; expand
;; Y-combinator
(lambda (f)
  ((lambda (x) (f (lambda (v) ((x x) v))))
   (lambda (x) (f (lambda (v) ((x x) v))))))


;; test cool
(((lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v))))))
  (lambda (length)
    (lambda (ls)
      (cond
        [(empty? ls) 0]
        [else (add1 (length (cdr ls)))]))))
 '(1 2 3))
