   
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
                          (applic (var even?) ((var e))))))
                 ((const #f) (const #f)))))