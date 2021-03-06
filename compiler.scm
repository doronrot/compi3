(load "parser.scm")

;TODO: 
;	remove old code lines	(marked with ;TODO; for our convenience)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Eliminate-Nested-Defines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eliminate-nested-defines
	(lambda (parsed_exp)
		(if (or (null? parsed_exp) (atom? parsed_exp))
			parsed_exp
			(if (is_lambda_exp? parsed_exp)	;are you lambda expression?
				(let ((eliminated_lambda_exp (eliminate-nested-defines-helper parsed_exp)))
				   (cons (car eliminated_lambda_exp) 
				   	     (eliminate-nested-defines (cdr eliminated_lambda_exp))))
				(cons (eliminate-nested-defines (car parsed_exp))
					  (eliminate-nested-defines (cdr parsed_exp)))))))

(define eliminate-nested-defines-helper
   (lambda (parsed_lambda_exp)
            (let* ((lambda_body (find_lambda_body parsed_lambda_exp)) ;returns inside list
            	   (defs_exps_list (build_list lambda_body (lambda (ds es) (list ds es))))
            	   (defs_list (car defs_exps_list)))
             	(if (null? defs_list)
             		parsed_lambda_exp
             		(let* ((applic_exp (create_applic defs_exps_list))
						   (lambda_type (car parsed_lambda_exp))
						   (lambda_vars (find_lambda_vars parsed_lambda_exp)))
             			`(,lambda_type ,@lambda_vars ,applic_exp))))))

(define create_applic
	(lambda (defs_exps_list)
		(let* ((defs_list (car defs_exps_list))
			   (exps_list (cadr defs_exps_list))
			   (sets_list (map create_set defs_list)) 
			   (args_list (map cadadr defs_list))		
			   (args_count (length defs_list))			
			   (lambda_simple_body `(seq ,(append sets_list exps_list)))
			   (falses_list (create_falses_list args_count))) 		
			`(applic (lambda-simple ,args_list
				,lambda_simple_body)
				,falses_list))))

(define create_set
	(lambda (def_element)
		`(set ,(cadr def_element) ,(caddr def_element))))

(define create_falses_list
	(lambda (args_count)
		(letrec ((recursive_create_falses_list
					(lambda (num acc)
						(if (equal? num 0)
							acc
							(recursive_create_falses_list (- num 1)
														  (cons `(const #f) acc))))))
			(recursive_create_falses_list  args_count  (list)))))
                
(define build_list
    (lambda (pes ret_des+exprs)
        (if (null? pes)
            (ret_des+exprs '() '())
            (build_list (cdr pes)
		                (lambda (ds es)
		                    (cond ((eq? (caar pes) 'def)
		                            (ret_des+exprs (cons (car pes) ds) es))
		                          ((eq? (caar pes) 'seq) 
		                            (build_list (cadar pes)
		                                 (lambda (ds1 es1)
		                                    (ret_des+exprs (append ds1 ds)
		                                                   (append es1 es)))))
		                          (else 
		                            (ret_des+exprs ds (cons (car pes) es)))))))))

(define is_lambda_exp?
	(lambda (parsed_exp)
		(if (null? parsed_exp)
			#f
			 (or (equal? (car parsed_exp) 'lambda-simple)
	      		 (equal? (car parsed_exp) 'lambda-opt)
				 (equal? (car parsed_exp) 'lambda-var)))))

(define find_lambda_body
	(lambda (lambda_expr)
		(if (equal? (car lambda_expr) 'lambda-opt)
			(cdddr lambda_expr)
            (cddr lambda_expr))))

(define find_lambda_vars
	(lambda (lambda_expr)
		(if (equal? (car lambda_expr) 'lambda-opt)
			(list (cadr lambda_expr) (caddr lambda_expr))  
			(list (cadr lambda_expr)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Remove-Applic-Lambda-Nil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define remove-applic-lambda-nil 
	(lambda (parsed_exp)
		(if (or (null? parsed_exp)(atom? parsed_exp))
			parsed_exp
			(if (is_redundant? parsed_exp)	;are you applic lambda-nil expression?
				(let ((lambda_nil_body (remove-applic-lambda-nil-helper parsed_exp))
					  (applic_args (find_applic_args parsed_exp)))
					(if(null? applic_args)
					   (remove-applic-lambda-nil  lambda_nil_body)
				   	   ;`(applic ,(remove-applic-lambda-nil  lambda_nil_body) ,applic_args)))		;TODO remove
					   parsed_exp))
				(cons (remove-applic-lambda-nil  (car parsed_exp))
					  (remove-applic-lambda-nil  (cdr parsed_exp)))))))

(define remove-applic-lambda-nil-helper
	(lambda (redundant_parsed_exp)
		(let* ((lambda_nil (cadr redundant_parsed_exp))
			   (lambda_nil_body (caddr lambda_nil)))
			lambda_nil_body)))

;changed 6.1
(define is_redundant?
	(lambda (parsed_exp)
		(and (equal? (car parsed_exp) 'applic)
			 (equal? (caadr parsed_exp) 'lambda-simple)
			 (equal? (cadadr parsed_exp) (list)))))

(define find_applic_args
	(lambda (applic_parsed_exp)
		(caddr applic_parsed_exp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Box-Set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define box-set 
	(lambda (parsed_exp)
		(if (or (null? parsed_exp)(atom? parsed_exp))
			parsed_exp
			(if (is_lambda_exp? parsed_exp)	;are you a lambda?
				(let ((boxed_lambda (box-set-helper parsed_exp)))
				   (cons (car boxed_lambda) 
				   	     (box-set (cdr boxed_lambda))))
				(cons (box-set (car parsed_exp))
					  (box-set (cdr parsed_exp)))))))
					  
(define box-set-helper
    (lambda (parsed_lambda_exp)
        (let* ((lambda_body (find_lambda_body parsed_lambda_exp)) ;returns inside list
               (lambda_vars (find_lambda_vars_op parsed_lambda_exp)) ;returns inside list
               (should_box_vars (filter (should_box_var? lambda_body) lambda_vars)))
            (if (null? should_box_vars)
                parsed_lambda_exp
                (let* ((boxed_body_exp (put_boxes should_box_vars lambda_body))
                	   (body_sets_and_boxes (create_set_box_body should_box_vars boxed_body_exp))
                       (lambda_type (car parsed_lambda_exp))
                       (lambda_vars_show (find_lambda_vars_show parsed_lambda_exp)))
                    `(,lambda_type ,@lambda_vars_show  ,body_sets_and_boxes))))))

;changed 6.1      
(define should_box_var?
    (lambda (lambda_body)
        (lambda (lambda_var)
	    	 (and (is_exist_get? lambda_body lambda_var)
	             (is_exist_set? lambda_body lambda_var)
	             (is_exist_bound? lambda_body lambda_var)))))
 
(define is_exist_get?
    (lambda (exp_part suspected_var)
         (cond ((null? exp_part) #f)
         	   ;is var
               ((equal? exp_part `(var ,suspected_var)) #t)
               ((atom? exp_part) #f)
               ;set exp
               ((equal? (car exp_part) 'set)
                     ;(equal? (cadadr exp_part) suspected_var)										;TODO remove
                  (is_exist_get? (caddr exp_part) suspected_var))
               ;lambda parameter
               ((and (is_lambda_exp? exp_part)
                     (member suspected_var (find_lambda_vars_op exp_part)))
                  #f)
               (else (or (is_exist_get? (car exp_part) suspected_var)
                         (is_exist_get? (cdr exp_part) suspected_var))))))
                    
(define is_exist_set?
     (lambda (exp_part suspected_var)
        (cond ((null? exp_part) #f)
              ((atom? exp_part) #f)              
              ((and (equal? (car exp_part) 'set) 
                    (equal? (cadadr exp_part) suspected_var))
              	 #t)
			  ((and (is_lambda_exp? exp_part)
                    (member suspected_var (find_lambda_vars_op exp_part)))
			     #f)
              (else (or (is_exist_set? (car exp_part) suspected_var)
                        (is_exist_set? (cdr exp_part) suspected_var))))))
                 
(define is_exist_bound?
     (lambda (exp_part suspected_var)
        (if (or (null? exp_part) (atom? exp_part))
        	#f
        	(if (is_lambda_exp? exp_part)
        		(is_exist_bound_helper? exp_part suspected_var)
        		(or (is_exist_bound? (car exp_part) suspected_var)
                	(is_exist_bound? (cdr exp_part) suspected_var))))))

(define is_exist_bound_helper?
    (lambda (exp_part suspected_var)
         (cond ((null? exp_part) #f)
               ((equal? exp_part `(var ,suspected_var)) #t)
               ((atom? exp_part) #f)
               ((and (equal? (car exp_part) 'set) 
                     (equal? (cadadr exp_part) suspected_var))
                  #t)
               ((and (is_lambda_exp? exp_part)
                     (member suspected_var (find_lambda_vars_op exp_part)))
                  #f)
               (else (or (is_exist_bound_helper? (car exp_part) suspected_var)
                         (is_exist_bound_helper? (cdr exp_part) suspected_var))))))

(define put_boxes
    (lambda (should_box_vars lambda_body)
        (if (null? should_box_vars)
            lambda_body
            (put_boxes (cdr should_box_vars)
                       (put_var_boxes lambda_body 
                       				  (car should_box_vars))))))

(define put_var_boxes
    (lambda (lambda_body should_box_var)       
        (let* ((with_set_boxes (put_set_boxes lambda_body should_box_var))
               (with_get_and_set_boxes (put_get_boxes with_set_boxes should_box_var)))
            with_get_and_set_boxes)))
        
;returns body with sets for this var
(define put_set_boxes
     (lambda (exp_part suspected_var)
        (cond ((or (null? exp_part) (atom? exp_part))
         		 exp_part)              
              ((and (equal? (car exp_part) 'set) 
                    (equal? (cadadr exp_part) suspected_var))
              	 (cons 'box-set
              		   ;(cdr exp_part)))
              		   (put_set_boxes (cdr exp_part) suspected_var)))
			  ((and (is_lambda_exp? exp_part)
                    (member suspected_var (find_lambda_vars_op exp_part)))
			     exp_part)
              (else (cons (put_set_boxes (car exp_part) suspected_var)
                          (put_set_boxes (cdr exp_part) suspected_var))))))

(define put_get_boxes
    (lambda (exp_part suspected_var)
         (cond ((null? exp_part) exp_part)
               ((equal? exp_part `(var ,suspected_var)) 
               	   (cons 'box-get
              		   (list exp_part)))
               ((atom? exp_part) exp_part)
               ((equal? (car exp_part) 'box-set) 
                   `(box-set
                     ,(cadr exp_part)
                  	 ,(put_get_boxes (caddr exp_part) suspected_var)))
               ((and (is_lambda_exp? exp_part)
                     (member suspected_var (find_lambda_vars_op exp_part)))
                  exp_part)
               (else (cons (put_get_boxes (car exp_part) suspected_var)
                           (put_get_boxes (cdr exp_part) suspected_var))))))

(define create_set_box_body 
	(lambda (should_box_vars boxed_body_exp)
      (flat_seq `(seq ,(append (map make_set should_box_vars)
								  boxed_body_exp)))))

(define make_set
	(lambda (var_to_box)
		`(set (var ,var_to_box) (box (var ,var_to_box))) ))

			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pe->Lex-Pe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pe->lex-pe
	(lambda (parsed_exp)
        (let ((with_pvar_and_bvar (pe->lex-pe-core parsed_exp)))
            (add_fvars with_pvar_and_bvar))))		

(define pe->lex-pe-core
	(lambda (parsed_exp)
		(if (or (null? parsed_exp)(atom? parsed_exp))
			parsed_exp
			(if (is_lambda_exp? parsed_exp)	;are you a lambda?
				(let ((lex_lambda (pe->lex-pe-helper parsed_exp)))
				   (cons (car lex_lambda) 
				   	     (pe->lex-pe-core (cdr lex_lambda))))
				(cons (pe->lex-pe-core (car parsed_exp))
					  (pe->lex-pe-core (cdr parsed_exp)))))))					  
					  
(define pe->lex-pe-helper
    (lambda (parsed_lambda_exp)
        (let* ((lambda_body (car (find_lambda_body parsed_lambda_exp))) ;returns inside list
               (lambda_vars (find_lambda_vars_op parsed_lambda_exp))) ;returns inside list
            (if (null? lambda_vars)
                parsed_lambda_exp
                (let* ((lex_body (update_vars lambda_vars lambda_body 0))
                       (lambda_type (car parsed_lambda_exp))
                       (lambda_vars_show (find_lambda_vars_show parsed_lambda_exp)))
                    `(,lambda_type ,@lambda_vars_show  ,lex_body))))))
                    
(define update_vars
    (lambda (lambda_vars lambda_body minor)
        (if (null? lambda_vars)
            lambda_body
            (update_vars (cdr lambda_vars)
                         (put_var_params_bounds lambda_body 
                                                (car lambda_vars) 
                                                minor)
                         (+ minor 1)))))
                                                
(define put_var_params_bounds
    (lambda (lambda_body var minor)       
        (let* ((with_pvar (put_pvar lambda_body var minor))
               (with_pvar_and_bvar (put_bvar with_pvar var minor)))
            with_pvar_and_bvar)))

;find all appearances of var as a current lambda parameter
(define put_pvar
    (lambda (exp_part var minor)	;lambda_body_exp
        (cond ((null? exp_part) exp_part)
              ((equal? exp_part `(var ,var))  
                  `(pvar ,var ,minor))
              ((atom? exp_part) exp_part)
              ((and (equal? (car exp_part) 'box-set)           
                    (equal? (cadadr exp_part) var))  
                `(box-set (pvar ,var ,minor) ,(caddr exp_part)))
              ; ((equal? (car exp_part) 'box)           											;TODO remove
              ;       exp_part)
              ((is_lambda_exp? exp_part)
                exp_part)
              (else (cons (put_pvar (car exp_part) var minor)
                          (put_pvar (cdr exp_part) var minor))))))

(define put_bvar
    (lambda (exp_part_pvar var minor)
        (if (or (null? exp_part_pvar) (atom? exp_part_pvar))
        	exp_part_pvar
        	(if (is_lambda_exp? exp_part_pvar)
        		(put_bvar_helper exp_part_pvar var -1 minor)
        		(cons (put_bvar (car exp_part_pvar) var minor)
                	  (put_bvar (cdr exp_part_pvar) var minor))))))

(define put_bvar_helper
    (lambda (exp_part_pvar var major minor)
         (cond ((null? exp_part_pvar) exp_part_pvar)
               ((equal? exp_part_pvar `(var ,var))
               		`(bvar ,var ,major ,minor))
               ((atom? exp_part_pvar) exp_part_pvar)
               ; ((and (equal? (car exp_part_pvar) 'box-set) 										;TODO remove
               ;       (equal? (cadadr exp_part_pvar) var))
               ;    `(bvar ,var ,major ,minor))
               ((and (is_lambda_exp? exp_part_pvar)
                     (member var (find_lambda_vars_op exp_part_pvar)))
                  exp_part_pvar)
               ((is_lambda_exp? exp_part_pvar) 
               		 (cons (put_bvar_helper (car exp_part_pvar) var (+ 1 major) minor)
                           (put_bvar_helper (cdr exp_part_pvar) var (+ 1 major) minor)))
               (else (cons (put_bvar_helper (car exp_part_pvar) var major minor)
                           (put_bvar_helper (cdr exp_part_pvar) var major minor))))))

(define add_fvars
    (lambda (exp_pbvars)
        (cond ((null? exp_pbvars) exp_pbvars)
        	  ((atom? exp_pbvars) exp_pbvars)
              ((equal? (car exp_pbvars) 'var)
                   `(fvar ,(cadr exp_pbvars)))
              (else (cons (add_fvars (car exp_pbvars))
                          (add_fvars (cdr exp_pbvars)))))))

(define find_lambda_vars_op
	(lambda (lambda_expr)
		 (cond ((equal? (car lambda_expr) 'lambda-opt)
				  `(,@(cadr lambda_expr) ,(caddr lambda_expr)))  
		 			; (append (cadr lambda_expr) (list (caddr lambda_expr))))						;TODO remove
			   ((equal? (car lambda_expr) 'lambda-simple)
					   (cadr lambda_expr))
			   (else (list (cadr lambda_expr))))))

(define find_lambda_vars_show
	(lambda (lambda_expr)
		(if (equal? (car lambda_expr) 'lambda-opt)
			(list (cadr lambda_expr) (caddr lambda_expr))  
			(list (cadr lambda_expr)))))
        
					  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Annotate-Tc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define annotate-tc
	(lambda (parsed_exp)
		(annotate-tc-helper parsed_exp #f)))

(define annotate-tc-helper
	(lambda (parsed_exp tp?)
		(cond ((or (null? parsed_exp) (atom? parsed_exp))
				 parsed_exp)
			  ;const var
			  ((member (car parsed_exp) '(pvar bvar fvar const))
			     parsed_exp)
			  ;applic
			  ((equal? (car parsed_exp) 'applic)
			  	 (if tp?
			  	 	 `(tc-applic ,@(annotate-tc-helper (cdr parsed_exp) #f))
			  	 	 `(applic ,@(annotate-tc-helper (cdr parsed_exp) #f))))
			  ;or
			  ((equal? (car parsed_exp) 'or)
			  	 (let* ((or_exps (cadr parsed_exp))
			  	 		(annotate_or_exps (annotate_or or_exps tp?)))
			  	    `(or ,annotate_or_exps)))
			  ;seq
			  ((equal? (car parsed_exp) 'seq)
			  	 (let* ((seq_exps (cadr parsed_exp))
			  	 		(annotate_seq_exps (annotate_seq seq_exps tp?)))
			  	    `(seq ,annotate_seq_exps)))
			  ;if
			  ((equal? (car parsed_exp) 'if3)
			  	 (let ((test (cadr parsed_exp))
			  	 	   (dit (caddr parsed_exp))
			  	 	   (dif (cadddr parsed_exp)))
			  	 	`(if3 ,(annotate-tc-helper test #f) 
			  	 		  ,(annotate-tc-helper dit tp?)
			  	 		  ,(annotate-tc-helper dif tp?))))
			  ;lambda
			  ((is_lambda_exp? parsed_exp)
			  	 (let ((lambda_type (car parsed_exp))
			  	 	   (lambda_vars (find_lambda_vars_show  parsed_exp))
			  	 	   (lambda_body (car (find_lambda_body  parsed_exp))))
			  	 	`(,lambda_type ,@lambda_vars ,(annotate-tc-helper lambda_body #t))))
			  ;def
			  ((equal? (car parsed_exp) 'def)
			  	 (let ((def_var (cadr parsed_exp))
			  	 	   (def_exp (caddr parsed_exp)))
			  	 	`(def ,def_var ,(annotate-tc-helper def_exp #f))))
			  ;set
			  ((or (equal? (car parsed_exp) 'set)
			  	   (equal? (car parsed_exp) 'box-set))
			  	 (let ((set_var (cadr parsed_exp))
			  	 	   (set_exp (caddr parsed_exp)))
			  	 	`(,(car parsed_exp) ,set_var ,(annotate-tc-helper set_exp #f))))
			  (else 
			  	 (cons (annotate-tc-helper (car parsed_exp) tp?)
			  	 	   (annotate-tc-helper (cdr parsed_exp) tp?)))
		)))

(define annotate_or
	(lambda (or_exps tp?)
		(if (null? (cdr or_exps))
			(annotate-tc-helper or_exps tp?)
			(cons (annotate-tc-helper (car or_exps) #f)
				  (annotate_or (cdr or_exps) tp?)))))

(define annotate_seq
	(lambda (seq_exps tp?)
		(if (null? (cdr seq_exps))
			(annotate-tc-helper seq_exps tp?)
			(cons (annotate-tc-helper (car seq_exps) #f)
				  (annotate_seq (cdr seq_exps) tp?)))))

