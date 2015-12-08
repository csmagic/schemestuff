(define (vars b) (map car b))
(define (exprs b) (map cadr b))
(define (letexpander b d)
;  `((lambda ,(vars b) ,d) . ,(exprs b)))
;   (cons
;    (cons 'lambda (cons (vars b) (cons d '())))
;    (exprs b)))
   (cons (list 'lambda (vars b) d) (exprs b)))

(define-macro (mylet bindings  body)
  (letexpander bindings body))
