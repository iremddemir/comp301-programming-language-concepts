#lang eopl

;; interpreter for the LET language. 


(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp

      ;; implement const-exp here
     (const-exp (num) (num-val num))

      ;; implement var-exp here
      (var-exp (var) (apply-env env var))

      ;; implement comp-exp here
      (comp-exp (exp1 op-exp exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env))
                      (val3 (value-of op-exp env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2))
                        (op (expval->string val3)))                
                     (cond ((equal? op "'greater'") (bool-val (> num1 num2)))
                          ((equal? op "'less'") (bool-val (< num1 num2)))
                          ((equal? op "'equal'")  (bool-val (= num1 num2)))
                          (else (display "unsportted operation"))))))

      
      ;; implement op-exp here
      (op-exp (exp1 op-exp exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env))
                      (val3 (value-of op-exp env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2))
                        (op (expval->string val3)))                
                     (cond ((equal? op "'add'") (num-val (+ num1 num2)))
                          ((equal? op "'mult'") (num-val (* num1 num2)))
                          ((equal? op "'sub'")  (num-val (- num1 num2)))
                          ((equal? op "'div'")  (num-val (/ num1 num2)))
                          (else (display "unsportted operation"))))))


      
      ;; if-exp
      (if-exp (cond1 exp1 else-exp)
              (let ((val1 (value-of cond1 env)))      
                (if (expval->bool val1)
                    (value-of exp1 env)
                    (value-of else-exp env))))

      ;; implement my-cond-exp here  (value-of last env)))
      (my-cond (cond1 exp1 conds exps else-exp)
               (cond
                ((null? conds) (if (expval->bool (value-of cond1 env)) (value-of exp1 env) (value-of else-exp env)))
                ((equal? (expval->bool (value-of cond1 env)) #t) (value-of (my-cond (car conds) (car exps) (cdr conds) (cdr exps) exp1) env))
                (else (value-of (my-cond (car conds) (car exps) (cdr conds) (cdr exps) else-exp) env))
                )
              )


      
      ;; implement str-exp here
      (str-exp (str) (str-val str))


      ;; implement bool-exp here
      (bool-exp (b) (if (equal? b "#true") (bool-val #t) (bool-val #f)))

      
      ;; implement zero-exp here
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))

      ;; implement let-exp here
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;;CUSTOM EXPRESSION checks whether first expression is a multiple of second expression
      (custom-exp (exp1 exp2)
         (let ((val1 (value-of exp1 env))
               (val2 (value-of exp2 env)))
                   (let ((num1 (expval->num val1))
                         (num2 (expval->num val2)))
                   (cond ((and (eq? 0 num2) (eq? 0 num1))(bool-val #t))
                         ((and (eq? 0 num2) (not (eq? 0 num1)))(bool-val #f))
                         ((eqv? (modulo num1 num2) 0)(bool-val #t))
                         (else (bool-val #f))))))
                                
)))


;(trace value-of)