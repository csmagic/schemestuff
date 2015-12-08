;   Arunachala Siva

; Written by rpm originally for pc scheme
; Modified (minimally) for cmuscheme 8-8-96
; Again 28-9-03

(define-macro (named-lambda name-paramlist . body)
  (let ((fname (car name-paramlist))
	(plist (cdr name-paramlist))
	)
    `(letrec ((,fname (lambda ,plist . ,body))) ,fname)))

(define (curry f) (lambda (x) (lambda (y) (f x y))))
(define (uncurry f) (lambda (x y) ((f x) y)))

(define (const x) (lambda (y) x))
(define (id x) x)
(define (compose2 f g) (lambda (x) (g (f x))))
(define (flip f) (lambda (x y) (f y x)))
(define (diagonalize f) (lambda (x) (f x x)))

;foldr used to be called reduce
(define (reduce f id)
  (named-lambda (reduce l)
      (cond ((pair? l) (f (car l) (reduce (cdr l))))
             ((null? l) id)
      )))

(define compose
  (lambda (f . fl)
    (lambda l
      (((reduce compose2 id) fl) (apply f l))
)))

(define (reduce132 f l) (lambda (id) ((reduce (uncurry f) id) l)))

(define cons# (curry cons))
(define map# (curry map))
(define apply# (curry apply))

(define (^ f) (lambda fl (lambda (x) (apply f (map (lambda (g) (g x)) fl)))))
(define ^list (^ list))

(define (preprocess f preprocessor) (lambda l
  (apply f (map preprocessor l))
))

(define (make-reduce car cdr null?)
  (define (reduce f id)
    (named-lambda (reduce l)
      (if (null? l) id
          (f (car l) (reduce (cdr l))))
    ))
  reduce
)

(define (take n) (lambda (l)
  (let take ((n n) (l l))
    (cond ((and (positive? n) (pair? l))
            `(,(car l) . ,(take (- n 1) (cdr l)))
            )
           ((or (zero? n) (null? l)) '())
     ))))

(define (drop n) (lambda (l)
  (let drop ((n n) (l l))
    (cond ((or (zero? n) (null? l)) l)
           ((and (positive? n) (pair? l)) (drop (- n 1) (cdr l)))
    ))))

(define (take-while p) (lambda (l)
  (let take-while ((l l))
    (cond ((pair? l) (if (p (car l)) `(,(car l) . ,(take-while (cdr l))) '()))
          ((null? l) '())
    ))))

(define (drop-while p) (lambda (l)
  (let drop-while ((l l))
    (cond ((pair? l) (if (p (car l)) (drop-while (cdr l)) l))
          ((null? l) '())
    ))))

(define (all p?)
  (named-lambda (all l)
    (or (null? l) (and (p? (car l)) (all (cdr l))))
  ))

(define (exists p?)
  (named-lambda (exists l)
    (and (pair? l) (or (p? (car l)) (exists (cdr l))))
  ))

(define (filter p?)
  (named-lambda (filter l)
    (cond ((null? l) '())
	  ((p? (car l)) `(,(car l) . ,(filter (cdr l))))
	  (else (filter (cdr l)))
    )))

(define (upto m n)
  (let upto ((n (- n 1))
             (a '())
             )
    (if (> m n) a (upto (- n 1) `(,n . ,a)))
    ))

(define (downto b a)
  (let downto ((a a)
          (acc '())
          )
     (if (>= a b) acc (downto (1+ a) `(,a . ,acc)))
    ))

(define (iota n) (upto 0  n))

(define (zip . l)
  (define allpair? (all pair?))
  (define mapcar (map# car))
  (define mapcdr (map# cdr))
  (define (zip l)
    (if (allpair? l)
        `(,(mapcar l) . ,(zip (mapcdr l)))
        '()
    ))
  (if (null? l) '() (zip l))
)

(define (zipwith f) (lambda l
  (map (apply# f) (apply zip l))
))
(define (implode l)
  (string->symbol ((reduce string-append "") (map symbol->string l)))
)

(provide 'basicStuff)
