;;; Arunachala Siva

(load "basicStuff.scm")
(define (match? pat item)
  (cond ((quoted-obj? pat) (equal? (quote-body pat)  item))
	((pair? pat)		;order sensitive
	 (and (pair? item)
	      (match? (car pat) (car item))
	      (match? (cdr pat) (cdr item))
	 ))
        ((symbol? pat) #t)
        ((null? pat) (null? item))
        ((constant-obj? pat)  (equal? pat item))

        (else #f)
   ))
;(set! (access match? user-global-environment) match?)

(define (quoted-obj? l)
    (and (pair? l)
         (eq? 'quote (car l))
         (pair? (cdr l))
         (null? (cddr l))
         ))

(define quote-body cadr)
(define (constant-obj? x) (or (string? x) (number? x) (char? x)))
(define trivial-pat? symbol?)
(define (wildcard? s) (eq? s '_))

(define (atom-paths tree)
  (let atom-paths
    ((tree tree) (current-path '()) (acc '()))
    (cond  ((quoted-obj? tree) acc)
           ((pair? tree)
            (atom-paths (cdr tree)
                        `(d . ,current-path)
                        (atom-paths (car tree) `(a . ,current-path) acc)
            ))
           ((null? tree) acc)
           ((symbol? tree)  (if (wildcard? tree)
                                acc
                               `((,tree ,current-path) . ,acc)
                               ))
          ((constant-obj? tree) acc)
         
	  (else (error "Unfit object for destruct" tree))
      )))

(define (make-fnc-call g)
  (define (A/Dlist->symbol l) (implode `(c ,@l r)))
  ((make-reduce (compose (take 4) A/Dlist->symbol)
                (drop 4)
                null?
                ) list g
   ))

(define (int-let-list stub) 
  (map# (^list car ((^ (make-fnc-call stub)) cadr))))
  
(define (destruct1 pat val expt expf)
    ;(val assumed to be a var, so may be reevaluated freely)
  
    (let*  ((outputlist ((int-let-list val) (atom-paths pat)))
	    (code-when-matches `(let ,outputlist ,expt)))
      (if (trivial-pat? pat)
        code-when-matches
        `(if (match? ',pat ,val)
             ,code-when-matches
             ,expf
         ))))
(define (destructf1 item pelist)
    (let* ((g (if (symbol? item) item (gentemp)))
	   (op (lambda (x y) 
                 (let ((match-action (if (= 1 (length (cdr x)))
                                         (cadr x)
                                         `(begin . ,(cdr x))
                                     )))
                   (destruct1 (car x) g match-action y)
                 )))
	   (reducer (reduce op `(error "destruct: failure"
                                      "\nitem\t\t: ",item
                                      "\nmatch-clauses: " ',(map car pelist)
                                      )))
           (body (reducer pelist))
          )
      (if (symbol? item) 
          body
          `(let ((,g ,item)) ,body)
     )))

(define-macro (destruct pat . body) (destructf1 pat body))

(provide 'destruct)
