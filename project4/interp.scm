(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################
        ;;PART-A array exps
        (newarray-exp (exp1 exp2)
           (let ((len (expval->num (value-of exp1 env)))
                (val (value-of exp2 env)))
             (arr-val (arr-elements (newarray-helper len val)))
             ))

        (update-array-exp (exp1 exp2 exp3)
           (let ((arr (expval->arr (value-of exp1 env)))
                 (ind (expval->num (value-of exp2 env)))
                 (val (value-of exp3 env)))
             (update-array-helper arr ind val)
           ))

        (read-array-exp (exp1 exp2)
           (let ((arr (expval->arr (value-of exp1 env)))
                 (ind (expval->num (value-of exp2 env))))
             (read-array-helper arr ind)))
        (print-array-exp (exp1)
                         (let ((arr (expval->arr (value-of exp1 env))))
                           (print-array-helper arr))
           )
        ;;PART-B stack exps
        (newstack-exp ()
                     (arr-val (arr-elements (newarray-helper max-stack-size empty-stack-val))) )
        (stack-push-exp (exp1 exp2)
                        (let ((stack (expval->arr (value-of exp1 env)))
                              (val (expval->num (value-of exp2 env))))
                          (stack-push-helper stack val))
                        )
        (stack-pop-exp (exp1)
                       (let ((stack (expval->arr (value-of exp1 env))))
                        (num-val (stack-pop-helper stack)))
                   )
        (stack-size-exp (exp1)
                        (let ((stack (expval->arr (value-of exp1 env))))
                      (num-val (stack-size-helper stack)))
                   )
        (stack-top-exp (exp1)
                       (let ((stack (expval->arr (value-of exp1 env))))
                         (num-val (stack-top-helper stack)))
                   )
        (empty-stack?-exp (exp1)
                (let ((stack (expval->arr (value-of exp1 env))))
                      (bool-val (eq? 0 (stack-size-helper stack))))
                      
                   )
        (print-stack-exp (exp1)
                         (let ((stack (expval->arr (value-of exp1 env))))
                           (print-stack-helper stack))
                   )
        ;;PART-C arraycomprehension exps
        (array-comprehension-exp (body var exp2)
                                 (let ((array (expval->arr (value-of exp2 env))))
                                  (arr-val (arr-comp-helper body var array env))
                                 ))

        ; #####################################################
        )))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE
  ;;initialize a stack with 0 val in 1000(allowed size) & 1 first element to keep track of the size
  (define max-stack-size 1001)
  (define empty-stack-val 0)

  ;;in our implementation, we had size information at the start just go get it
  (define stack-size-helper
    (lambda (stack)
      (read-array-helper stack 0)))

  ;;update the value at the end+1(first empty element) of the stack & update end-of-stack index
  (define stack-push-helper
    (lambda (stack val)
      (let ((ind (+ 1 (stack-size-helper stack))))
        
         (update-array-helper stack ind val)
         (update-array-helper stack 0 ind)))

      )
   (define stack-pop-helper
     (lambda (stack)
      (let ((ind (stack-size-helper stack)))
         (let ((pop-val (read-array-helper stack ind)))
           (cond ((> ind 0)
         (update-array-helper stack ind 0)
         (update-array-helper stack 0 (- ind 1))
          pop-val)
           (else -1))
          ))))
  ;;get the value at the end of the index
  (define stack-top-helper
     (lambda (stack)
      (let ((ind (stack-size-helper stack)))
        (read-array-helper stack ind))))

  (define print-stack-helper
    (lambda (arr)
      (cases arrval arr
        (arr-elements (array) (print-stack-helper-rec array (stack-size-helper arr) 0 ))
      )))

  (define print-stack-helper-rec
    (lambda (arr size index)
      (cond ((eq? index 0) (display "stack size: ") (display size) (display "  stack: ")(print-stack-helper-rec (cdr arr) size (+ 1 index)))
            ((>= size index) (display (deref (car arr))) (display " ") (print-stack-helper-rec (cdr arr) size (+ 1 index)))
            (else (display " ")))))
    
  
  (define arr-comp-helper
    (lambda (body var array env)
      (arr-comp-helper-rec body var array env (array-length array) 0)))

  (define arr-comp-helper-rec
    (lambda (body var arr-list env size ind)
      (cond ((null? arr-list) arr-list)
            ((eq? size ind) arr-list)
            (else
              (update-array-helper arr-list ind (value-of body (extend-env var (read-array-helper arr-list ind) env)))
               (arr-comp-helper-rec body var arr-list env  size (+ 1 ind))))))
  
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )


