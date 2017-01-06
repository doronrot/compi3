   
;;;;;;;;;;;;;;;;;;;;;;;;   annotate tc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(x 
  (lambda (x) (x 
                (lambda () (x 
                             (lambda () (x x))
                             )))))

(annotate-tc 
 (pe->lex-pe
  (box-set 
    (remove-applic-lambda-nil 
      (eliminate-nested-defines 
          (parse  '


(lambda () 
     (define foo 
        (lambda (x) 
           (set! foo 4)))
     (foo 1))


            ))))))




(define my-even? 
  (lambda (e) 
    (define even? 
      (lambda (n) 
        (or (zero? n) (odd? (- n 1))) )) 
    #t))

 (annotate-tc 
    '(or ((applic (fvar zero?) ((pvar n 0)))
        (applic (fvar odd?) ((applic (fvar -) ((pvar n 0) (const 1))))))))


 (lambda (x y) ((lambda (a b c) a b c) 5 6 7) x y)

 (or (zero? n) (odd? (- n 1)))

(lambda (x y) 
  (lambda (z) 
    (set! x x) 
    (set! y 2)))


(let* ((c 0) (a (box c)))
  (list (lambda () a) 
        (lambda () 
           (set! a (+ a 1)))
        (lambda (b) 
           (set! a b))))

(lambda (a b c d e f g h i j k lmnop q r stuv w . xyz)
  xyz 
  (lambda (k j) 
     ((lambda () j) k))
  (i h) 
  g 
  f 
  free-var1 
  (e d c b ((((a))))))

(lambda (a b c d e f g h i j k lmnop q r stuv w . xyz)
  xyz)

((lambda () j) k)


;;;;; last test bli neder ;;;;;
 (lambda (a b c) 
   (begin 
     (begin (begin (define foo 
                          (lambda (x) 
                             (set! foo 4) 
                             (+ x x))) 
                        (define goo 
                           (lambda (y) 
                              (define foo 
                                 (* a b)) (* foo b)))))
     (define t5 
       (lambda (x) 
          (begin (define x 6) 
                 (set! goo 5) 
                 (set! f 50) (* a x)))) 
     (begin (define f 
              (lambda (d) 
                 (* d d d))) 
            (begin (define h 
                        (lambda (f g) 
                           (if (or (even? g) (even? g) (even (+ f g))) f g)))))) 
   (* (foo (goo (* a x))) (f (* b c))))



(lambda (a b c) 
   (begin 
     (begin (begin (define foo 
                          (lambda (x) 
                             (set! foo 4) 
                             (+ x x))) 
                        ; (define goo 
                        ;    (lambda (y) 
                        ;       (define foo 
                        ;          (* a b)) (* foo b)))
                        5
                        )))
   (* (foo (goo 1)) 1))

;bt
(lambda-simple
  ()
  (applic
    (lambda-simple
      (foo)
      (seq ((set (var foo)
                 (lambda-simple (x) (set (var foo) (const 4))))
            (applic (var foo) ((const 1))) )) )
    ((const #f))))

(is_exist_get? bt1 'foo)

(lambda-simple
      (foo)
      (seq ((set (var foo)
                 (lambda-simple (x) (set (var foo) (const 4))))
            (applic (var foo) ((const 1))) )) )






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  pbf vars   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add_fvars (put_pvar (find_lambda_body (box-set (remove-applic-lambda-nil (parse '(lambda (a) (set! a 3)
            a
            (lambda () a))))))
          'a
          0))

(pe->lex-pe
  (box-set 
    (remove-applic-lambda-nil 
      (eliminate-nested-defines 
          (parse 

        '
 (lambda (x) (set! x 6) (lambda (y) x))

          ))))))


 (lambda (x) (set! x 6) (lambda (y) x))

  (pe->lex-pe (parse '(lambda (a . d) a d e)))

   (pe->lex-pe '(set! x (f 1)))

 (find_lambda_vars_op (parse '(lambda (a . d) a)))

(put_pvar  (parse '(lambda (a) (set! a 3)
            a
            (lambda () a)))
          'a)


(a)

(find_lambda_vars (parse '(lambda (a b) a)))

(find_lambda_vars (parse '(lambda (a . b) a)))


;;;;;; flat seq  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(parse '(define foo
            (lambda (a b c)
              (begin 1 2 (begin 3 (begin 4) 5 (begin (begin 6 7) 8 9)) 10))))

(def (var foo)
     (lambda-simple
       (a b c)
       (seq ((const 1)
              (const 2)
              (seq ((const 3)
                     (const 4)
                     (const 5)
                     (seq ((seq ((const 6) (const 7)))
                            (const 8)
                            (const 9)))))
              (const 10)))))

(define flat_begin
  (lambda (lst)
    (cond 
    	((null? lst) 
    		'())
    	((and (list? (car lst)) (equal? (caar lst) 'seq))
    		`(,@(flat_begin(cdar lst)) ,@(flat_begin (cdr lst))))
    	((equal? (car lst) 'seq) 
    		(flat_begin (cdr lst)))
    	(else 
    		`(,(car lst) ,@(flat_begin (cdr lst)))))))

 ex1
(def (var foo)
     (lambda-simple
       (a b c)
       (seq ((const 1)
              (const 2)
              (seq ((const 3)
                     (const 4)
                     (const 5)
                     (seq ((seq ((const 6) (const 7)))
                            (const 8)
                            (const 9)))))
              (const 10)))))

ex2
(seq ((const 1) (const 2) (const 3)))

ex22
(seq ((const 0) (seq ((const 1) (const 2) (const 3)))))

ex3
(lambda-simple
  (a b)
  (seq ((const 1)
         (const 2)
         (seq ((const 3) (const 4) (const 5)))
         (const 6)
         (const 7))))
ex4
(seq ((const 1)
              (const 2)
              (seq ((const 3)
                     (const 4)
                     (const 5)
                     (seq ((seq ((const 6) (const 7)))
                            (const 8)
                            (const 9)))))
              (const 10)))

ex6 to parse 
'(define goo
	(lambda args
		(let ((x 5) 
			  (y 6)
			  (z args))
		   (begin 1 2 (+ 1 2) 3 (begin 4 (begin 5 6 (begin) (begin) (begin 7)) 8 9) 10 11))))

'(begin 1 (+ 2 (begin 3 4)))
'(begin 1 2 (if x (begin 3 4))
(begin 1 (and q (begin 3 (begin  4 5) 5 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  box   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  lambda-nil   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(remove-applic-lambda-nil
'(applic
(lambda-simple
(fact)
(seq ((set (var fact) (box (var fact)))

(box-set
(var fact)
(lambda-simple
(n)
(if3 (applic (var zero?) ((var n)))
(const 1)

(applic
  (var *)
  ((var n)
    (applic
      (box-get (var fact))
      ((applic (var -) ((var n) (const 1))))))))))
(applic
  (lambda-simple () (applic (box-get (var fact)) ((const 5))))
  ()) 
)))
((const #f))
))


(applic
  (lambda-simple () 1)
  ())


(remove-applic-lambda-nil '(applic (lambda-simple () (applic (lambda-simple () 1) ())) ()))
  ;#t
(is_redundant? '(applic (lambda-simple (a) 1) ()))
  ;#f
(is_redundant? '(applic (lambda-simple () 1) ((const 2))))
  ;#f but eyzlenu returns #t
  ;we didn't take care of that because it's not valid lambda syntax
(is_redundant? '((lambda-simple (a) 1) ()))
  ;#f
(is_redundant? '(applic (lambda-var (a) 1) ()))
  ;#f

(remove-applic-lambda-nil (parse '(lambda () a (lambda () (lambda () b)))))

(remove-applic-lambda-nil (parse '((lambda a a))))

(remove-applic-lambda-nil (parse '((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () (+)))))))))))))))))

(remove-applic-lambda-nil (parse   '(((lambda () f)) ((lambda () g)) ((lambda () h)) (z (m c (d ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () (+)))))))))))))))))))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  nested-defines   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *test-expr*
    '(define my-even? (lambda (e)
                            (define even? (lambda (n) (or (zero? n) (odd? (- n 1)))))
                            (define odd?  (lambda (n) (and (positive? n) (even? (- n 1)))))
                            (even? e))))    
 
                           
> (parse *test-expr*)
(def (var my-even?)
      (lambda-simple (e)
            (seq ((def (var even?)
                        (lambda-simple (n)
                            (or ((applic (var zero?) ((var n)))
                                 (applic (var odd?)  ((applic (var -) ((var n) (const 1)))))))))
                  (def (var odd?)
                        (lambda-simple (n)
                            (if3 (applic (var positive?) ((var n)))
                                 (applic (var even?) ((applic (var -) ((var n) (const 1)))))
                                 (const #f))))
                  (applic (var even?) ((var e)))))))

(seq ((const 1)
      (const 2)))
                  
> (eliminate-nested-defines
(parse *test-expr*))
    
(def (var my-even?)
     (lambda-simple (e)
        (applic (lambda-simple (even? odd?)
                    (seq ((set (var even?)
                               (lambda-simple (n)
                                  (or ((applic (var zero?) ((var n)))
                                       (applic (var odd?) ((applic (var -) ((var n) (const 1)))))))))
                          (set (var odd?)
                               (lambda-simple (n)
                                  (if3 (applic (var positive?) ((var n)))
                                       (applic (var even?) ((applic (var -) ((var n) (const 1)))))
                                       (const #f))))
                          (applic (var even?) ((var e))))) )
                 ((const #f) (const #f)) )  ))




////////////////////////////////////////////////////////
(eliminate-nested-defines '(def (var my-even?)
        (lambda-simple (e)
              (seq ((def (var even?)
                          (lambda-simple (n)
                              (or ((applic (var zero?) ((var n)))
                                   (applic (var odd?)  ((applic (var -) ((var n) (const 1)))))))))
                    (def (var odd?)
                          (lambda-simple (n)
                              (if3 (applic (var positive?) ((var n)))
                                   (applic (var even?) ((applic (var -) ((var n) (const 1)))))
                                   (const #f))))
                    (applic (var even?) ((var e))))))))

;/// winterfeld
(eliminate-nested-defines (parse '(lambda (a . b) (define x 1) 1)))

(eliminate-nested-defines '(def (var a) (lambda-opt () (seq ((const 1)(const 2))))))



(define create_set
  (lambda (def_element)
    `(set (cadr def_element) (caddr def_element))))


((def (var a) (const 1)) (def (var b) (const 2)) (def (var c)(const 3)))

((const 10)(const 20))

(((def (var a) (const 1)) (def (var b) (const 2)) (def (var c)(const 3)))
 ((const 10)(const 20)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
