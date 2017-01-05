   

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

          )))))


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
