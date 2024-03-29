(module lang (lib "eopl.ss" "eopl")                

  ;; language for EXPLICIT-REFS
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      (expression
       ("proc" "(" identifier ")" expression)
       proc-exp)

      (expression
       ("(" expression expression ")")
       call-exp)

      (expression
        ("letrec"
          (arbno identifier "(" identifier ")" "=" expression)
           "in" expression)
        letrec-exp)
      
      ;; new for explicit-refs

      (expression
        ("begin" expression (arbno ";" expression) "end")
        begin-exp)

      (expression
        ("newref" "(" expression ")")
        newref-exp)

      (expression
        ("deref" "(" expression ")")
        deref-exp)

      (expression
        ("setref" "(" expression "," expression ")")
        setref-exp)

      ; #####################################################
      ; ###### ENTER YOUR CODE HERE
      ; ###### define the grammar definitions for new expressions here
      ; #####################################################
      ;;PART-A array
      (expression
       ("newarray" "(" expression "," expression ")")
         newarray-exp)
      (expression
       ("update-array" "(" expression "," expression "," expression ")")
         update-array-exp)
      (expression
       ("read-array" "(" expression "," expression ")")
         read-array-exp)
      (expression
       ("print-array" "(" expression ")")
         print-array-exp)
      ;;PART-B stack
      (expression
       ("newstack""("")") newstack-exp)
      (expression
       ("stack-push" "(" expression "," expression ")" ) stack-push-exp)
      (expression
       ("stack-pop" "(" expression ")" ) stack-pop-exp)
      (expression
       ("stack-size" "(" expression ")" ) stack-size-exp)
      (expression
       ("stack-top" "(" expression ")" ) stack-top-exp)
      (expression
       ("empty-stack?" "(" expression ")" ) empty-stack?-exp)
      (expression
       ("print-stack" "(" expression ")" ) print-stack-exp)
       ;;PART-C array comprehension
      (expression
       ("[" expression "for" identifier "in" expression "]" ) array-comprehension-exp)

      ; #####################################################

      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
