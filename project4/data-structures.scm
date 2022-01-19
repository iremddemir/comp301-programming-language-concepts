(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (ref-val
      (ref reference?))
    ; #####################################################
    ; ###### ENTER YOUR CODE HERE
    ; ###### add a new value type for your arrays
    ; #####################################################
    (arr-val
     (arr arrval?))

    ; #####################################################
    )

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

  ;; HINT if you need extractors, add them here
  (define expval->arr
    (lambda (v)
      (cases expval v
        (arr-val (arr) arr)
        (else (expval-extractor-error 'arr v)))))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ; #####################################################
  ; ###### ENTER YOUR CODE HERE
  ; ###### you might want to add a new datatype for arrays here similar 
  ; ###### to mutable pairs.
  ; #####################################################
  (define-datatype  arrval arrval?
    (arr-elements
     (elements (list-of reference?)))
  )
  ; #####################################################

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec*
      (proc-names (list-of symbol?))
      (b-vars (list-of symbol?))
      (proc-bodies (list-of expression?))
      (saved-env environment?)))

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym (expval->printable val))
	    (env->list saved-env)))
	(extend-env-rec* (p-names b-vars p-bodies saved-env)
	  (cons
	    (list 'letrec p-names '...)
	    (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	(proc-val (p)
	  (cases proc p
	    (procedure (var body saved-env)
	      (list 'procedure var '... (env->list saved-env)))))
	(else val))))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE
  (define newarray-helper
    (lambda (length value)
      (cond ((= length 0) '())
          (else (cons (newref value) (newarray-helper (- length 1) value))))))

  (define update-array-helper
    (lambda (arr index value)
      (cases arrval arr
      (arr-elements (array) (setref! (list-ref array index) value)))))

  (define read-array-helper
    (lambda (arr index)
      (cases arrval arr
      (arr-elements (array) (deref (list-ref array index))))))

  (define print-array-helper
    (lambda (arr)
      (cases arrval arr
        (arr-elements (array) (display "[ ") (print-array-helper-rec array))
      )))

  (define print-array-helper-rec
    (lambda (arr)
      (cond ((null? arr) (display "]"))
         (else (display (expval->num (deref (car arr)))) (display " ") (print-array-helper-rec (cdr arr))))))

  ;;array-length calculator for array comparision function
  (define array-length
    (lambda (arr)
      (cases arrval arr
        (arr-elements (array) (array-length-calculate array 0)))))
  
  (define array-length-calculate
    (lambda (arr size)
      (cond ((null? arr) size)
            (else (array-length-calculate (cdr arr) (+ 1 size))))))
       

)
