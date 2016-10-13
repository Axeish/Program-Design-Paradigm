;; Problem Set 12 : eval.rkt


;; Language 
;;------------------------------------------------------------------------
#lang racket

(require "extras.rkt")
(require rackunit)
(require racket/list)
(define TIME-ON-TASK 17)

;; Provide Functions
;;------------------------------------------------------------------------

(provide mk-Program%)
(provide mk-Expr%)
(provide Program<%>)
(provide Expr<%>)
(provide Result<%>)

;; Global Constants
;;------------------------------------------------------------------------

(define ZERO 0)
(define MT-LIST '())
(define TRUE true)
(define FALSE false)


;;------------------------------------------------------------------------

;; A UniqueListOf<X> is a ListOf<X>
;; WHERE: none of the elements of the list are equal? to each other

;; Examples
(define UniqueListEx1 (list 1 2))
(define UniqueListEx2 (list 'a 'b 'c))

;; Template
;; uniqueList-fn : UniqueListOf<X> -> ???

;; (define (uniqueList-fn ls)
;;   (cond 
;;     [(empty? ls) ...]
;;     [else (...(first ls)...(uniqueList-fn (rest ls))...)]))

;;------------------------------------------------------------------------------

;; A 2ListOf<X> is a (cons X (cons X ListOf<X>))
;; Represents a list of 2 or more elements.

;; Examples
(define 2ListOfEx1 (list 1 2))
(define 2ListOfEx2 (list 'a 'b 'c))

;; Template
;; 2ListOf-fn : 2ListOf<X> -> ???

;; (define (2ListOf-fn ls)
;;   (cond 
;;     [(empty? (rest (rest ls))) (...(first ls)...(second ls)...)]
;;     ; (second ls) left out to ensure atleast 2 elements are passed into 
;;     ; 2ListOf-fn
;;    [else (...(first ls)...(2ListOf-fn (rest ls))...)]))

;;------------------------------------------------------------------------------

;; A Def is a (make-def FnName UniqueListOf<Param> Expr)
;; Represents the definition of a function with the specified parameters.
(define-struct def (name params body) #:transparent)

;; Example

(define DEF (make-def 'f '(x) 'x))

;; Template
;; def-fn : Def -> ???
;; (define (def-fn d)
;;   (...(Fn-name-fn (def-name d))...
;;       ...(param-ls-fn (def-params d))...
;;       ...(expr-fn (def-body d))...))

;;------------------------------------------------------------------------------

;; A Program is a:
;; - empty
;; - (cons Expr Program)
;; - (cons Def Program)
;; Represents a PDPLang program consisting of function defs and expressions.
;; WHERE: no two Defs have the same name

;; Examples
(define ProgramEx (list DEF 2 "error" true))

;; Template
;; program-fn : Program -> ???
;; (define (program-fn p)
;;   (cond
;;     [(empty? p) ...]
;;     [(not (def? (first p))) (...(expr-fn (first p))...
;;                                 (program-fn (rest p))...)]
;;     [else (...(def-fn (first p))...(program-fn (rest p))...)]))

;;------------------------------------------------------------------------------

;; An ErrString is a String, representing a PDPLang error message.

;; Example

(define ERROR "error")

;; Predicate
;; errstr? : Expr -> Boolean
;; Returns true if e is a PDPLang ErrString expression.
;; Strategy: Function composition
(define (errstr? e) (string? e))

;;------------------------------------------------------------------------------

;; A Lambda is a (make-lam UniqueListOf<Param> Expr)
;; Represents a lambda expression in PDPLang
(define-struct lam (params body) #:transparent)

;; Example
(define LAM-EX (make-lam '(x) 'x))

;; Predicate
;; lambda? : Expr -> Boolean
;; Returns true if e is a PDPLang lambda expression.
;; Strategy: Function composition
(define (lambda? e) (lam? e))

;;------------------------------------------------------------------------------

;; A Var is a Symbol, representing PDPLang variable.

;; Examples
(define VAR1 'x)
(define VAR2 'fn)

;; Predicate
;; var? : Expr -> Boolean
(define (var? e) (symbol? e))

;;------------------------------------------------------------------------------

; An Expr is one of:
; - Number
; - Boolean
; - Var
; - ErrString
; - Lambda
; - (make-arith ArithOp 2ListOf<Expr>) ; an arithmetic expression
; - (make-bool BoolOp 2ListOf<Expr>)   ; a boolean expression
; - (make-cmp CmpOp 2ListOf<Expr>)     ; a comparison expression
; - (make-if-exp Expr Expr Expr)       ; an if conditional
; - (make-call Expr ListOf<Expr>)      ; a function call
; Represents a PDPLang expression.
(define-struct arith (op args) #:transparent)
(define-struct bool (op args) #:transparent)
(define-struct cmp (op args) #:transparent)
(define-struct if-exp (test branch1 branch2) #:transparent)
(define-struct call (fn args) #:transparent)


;; Examples

(define EXPR1 28)
(define EXPR2 true)
(define EXPR3 (make-arith '+ (list 1 2 3)))
(define EXPR4 (make-if-exp true 2 3))
(define EXPR5 (make-call LAM-EX (list 2)))

;; Template
;; expr-fn : Expr -> ???

;; (define (expr-fn ex)
;;   (cond
;;     [(number? ex) ...]
;;     [(boolean? ex) ...]
;;     [(var? ex) (...(var-fn ex)...)]
;;     [(errstr? ex) (...(errstr-fn ex)...)]
;;     [(lambda? ex) (...(params-fn (lam-params ex))...
;;                       (expr-fn (lam-body ex))..)]
;;     [(arith? ex) (...(arithOp-fn (arith-op ex))...
;;                      (expr-list-fn (arith-args ex))...)]
;;     [(bool? ex) (...(boolOp-fn (bool-op ex))...
;;                     (expr-list-fn (bool-args ex))...)]
;;     [(cmp? ex) (...(cmpOp-fn (cmp-op ex))...
;;                    (expr-list-fn (cmp-args ex))...)]
;;     [(if-exp? ex) (...(expr-fn (if-exp-test ex))...
;;                       (expr-fn (if-exp-branch1 ex))...
;;                       (expr-fn (if-exp-branch2 ex))...)]
;;     [(call? ex) (...(expr-fn (call-fn ex))...
;;                     (expr-list-fn (call-args ex))...)]))

;;------------------------------------------------------------------------------

;; A Param is a Var, representing a function parameter.

;; An ArithOp is one of:
;; - '+
;; - '-
;; - '*
;; - '/
;; Represents an arithmetic operation in PDPLang

;; <Param predicates> : Param -> Boolean
;; Returns true if op is the Param indicated by the function name.
;; STRATEGY: Function composition
(define (plus? op) (symbol=? '+ op))
(define (minus? op) (symbol=? '- op))
(define (mult? op) (symbol=? '* op))
(define (div? op) (symbol=? '/ op))

;; Template
;; (define (arithOp-fn op)
;;   (cond
;;     [(plus? op) ...]
;;     [(minus? op) ...]
;;     [(mult? op) ...]
;;     [(div? op) ...]))

;;------------------------------------------------------------------------------

;; A BoolOp is one of:
;; - 'and
;; - 'or
;; Represents a boolean operation in PDPLang

;; <BoolOp predicates> : BoolOp -> Boolean
;; Returns true if op is the BoolOp indicated by the function name.
;; STRATEGY: Function composition
(define (and? op) (symbol=? 'and op))
(define (or? op) (symbol=? 'or op))

;; Template 
;; boolOp-fn : BoolOp -> ???
;; (define (boolOp-fn b)
;;   (cond
;;     [(and? b) ...]
;;     [(or? b) ...]))

;;------------------------------------------------------------------------------

;; A CmpOp is one of:
;; - '=
;; - '<
;; - '>
;; Represents a comparison operation in PDPLang

;; <CmpOp predicates> : CmpOp -> Boolean
;; Returns true if op is the CmpOp indicated by the function name.
;; STRATEGY: Function composition
(define (comp-eq? op) (symbol=? '= op))
(define (comp<? op) (symbol=? '< op))
(define (comp>? op) (symbol=? '> op))

;; Template
;; cmpOp-fn : CmpOp -> ???
;; (define (cmpOp-fn c)
;;   (cond
;;     [(comp-eq? c) ...]
;;     [(comp<? c) ...]
;;     [(comp>? c) ...]))

;;------------------------------------------------------------------------------

;; A FnName is a Var, representing a function name.

;; Example:
(define FnNameEx 'func)

;;------------------------------------------------------------------------------

; A Result is a:
; - Number
; - Boolean
; - ErrString
; - Lambda

;; Examples
(define RESULT1 28)
(define RESULT2 true)
(define RESULT3 "error")
(define RESULT4 (make-lam '(x) 'x))

;; Template
;; result-fn : Result -> ???
;; (define (result-fn r)
;;   (cond
;;     [(number? r) ...]
;;     [(boolean? r) ...]
;;     [(errstr? r) (...(errstr-fn ex)...)]
;;     [(lambda? r) (...(params-fn (lam-params r))...
;;                      (expr-fn (lam-body r))..)]))

;;------------------------------------------------------------------------------

;; A StaticDist is a (list Depth Index)
;; Represents a variable reference
;; where depth is number of additional lambdas between this var ref and the
;; lambda for which this variable is a parameter,
;; and index is the (0-based) position of this variable in that lambda's
;; parameter list.

;; Examples
(define SD1 (list 1 0))
(define SD2 (list 0 0))

;; Template
;; staticDist-fn : StaticDist -> ???
;; (define (staticDist-fn sd)
;;   (...(first sd)...(second sd)...))

;;------------------------------------------------------------------------------

;; A Depth is a Natural

;;------------------------------------------------------------------------------

;; An Index is a Natural

;;------------------------------------------------------------------------------

;; A LamNoVar is a (make-lam/no-var ExprNoVar)
(define-struct lam/no-var (body) #:transparent)

;; Example
(define LAM-NO-VAR (make-lam/no-var SD1))

;; Template
;;(define (lamNoVar-fn lm)
  ;;(...(expr/no-var-fn (lam/no-var-body lm))...))

;;------------------------------------------------------------------------------

;; An ExprNoVar is one of:
;; - Number
;; - Boolean
;; - StaticDist
;; - ErrString
;; - LamNoVar
;; - (make-arith ArithOp 2ListOf<ExprNoVar>) ; an arithmetic expression
;; - (make-bool BoolOp 2ListOf<ExprNoVar>)   ; a boolean expression
;; - (make-cmp CmpOp 2ListOf<ExprNoVar>)     ; a comparison expression
;; - (make-if-exp ExprNoVar ExprNoVar ExprNoVar) ; an if conditional
;; - (make-call ExprNoVar ListOf<ExprNoVar>) ; a function call
;; Represents an Expr without explicit variables.

;; Examples

(define EXPR-NOVAR1 (make-lam/no-var (list 0 0)))
(define EXPR-NOVAR2 28)
(define EXPR-NOVAR3 true)

;; Template
;; expr-fn : ExprNoVar -> ???

;; (define (expr/no-var-fn ex)
;;   (cond
;;     [(number? ex) ...]
;;     [(boolean? ex) ...]
;;     [(list? ex) (...(staticDist-fn ex)...)]
;;     [(errstr? ex) (...(errstr-fn ex)...)]
;;     [(lam/no-var? ex) (...(expr/no-var-fn (lam/no-var-body ex))...)]
;;     [(arith? ex) (...(arithOp-fn (arith-op ex))...
;;                      (expr/no-var-list-fn (arith-args ex))...)]
;;     [(bool? ex) (...(boolOp-fn (bool-op ex))...
;;                     (expr/no-var-list-fn (bool-args ex))...)]
;;     [(cmp? ex) (...(cmpOp-fn (cmp-op ex))...
;;                    (expr/no-var-list-fn (cmp-args ex))...)]
;;     [(if-exp? ex) (...(expr/no-var-fn (if-exp-test ex))...
;;                       (expr/no-var-fn (if-exp-branch1 ex))...
;;                       (expr/no-var-fn (if-exp-branch2 ex))...)]
;;     [(call? ex) (...(expr/no-var-fn (call-fn ex))...
;;                     (expr/no-var-list-fn (call-args ex))...)]))

;;------------------------------------------------------------------------------



; A Program<%> represents an interface that is imlemneted by Program% class
; which contains the program object

(define Program<%>
  (interface()
    
    ; eval : -> ListOf<Result<%>>
    ; Evaluates expressions in the program to Result<%>s
    ; WHERE: A function may be called before it is defined.
    ; WHERE: The results have the same order as their original expressions in the program.
    eval
    
    
    ; eval-with-defns : ListOf<Def> -> ListOf<Result<%>>
    ; Evaluates expressions in the program to Result<%>s with the list of all the defs in
    ; the program
    ; WHERE: A function may be called before it is defined.
    ; WHERE: The results have the same order as their original expressions in the program.
    eval-with-defns
    
    
    ; gather-defns :  -> ListOf<Def>
    ; Extracts all the defs in the programs and maps them in a list
    ; to be used for evaluation of the program
    gather-defns
    
    ))




; A Expr<%> represents an interface for Expression and is
; implemented by the various kinds of expressions in the 
; program
(define Expr<%>
  (interface()
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    eval
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    subst
       
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr.
    to-expr
    
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    to-expr/no-var
    
    
    ; to-expr/no-var/lst : ListOf<ListOf<UniqueParams>> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; by keeping  atrack of 
    to-expr/no-var/lst
    
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given Expr<%>
    expr=?
    ))


(define Result<%>
  (interface()
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    ; If Result is a lambda, applies it to given arguments, else errors
    app
    
    ))


; A Arith% is a (new Arith% [op ArithOp] [Args 2ListOf<Expr%>]
; Represents the Arith class that implements the Expr<%> interface
; INTERP: 
; op : Contains mathematical operator + , _ , * , /
; args : Contains a 2ListOf<Expr%> which can further be simplified and
; finally evaluates the given arithematic expression
(define Arith% 
  (class* object% (Expr<%>)
  (init-field op args)
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    (define/public (eval defs)
      (eval-arith op (eval-expr-ls args defs)))
    
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    (define/public (subst r x)
      (new Arith% [op op] 
                 [args (map (λ (ex) (send ex subst r x)) args)]))
    
    
    ; eval-arith : ArithOp 2ListOf<Result<%>> -> Result<%>
    ; Performs the arithmetic operation op on args, if all elements of args are 
    ; numbers
    ; Strategy: Data decomposition on args: 2ListOf<Result<%>>
    (define (eval-arith op args)
      (local (; checking if all args are numbers
              (define is-valid (andmap (λ (x)
                                         (number? (send x to-expr))) args)))
        ;--IN--
        (if is-valid
            (calculate-arith op (first args) (rest args))
            (new ErrString% [ex "err: ArithExpr expects evaluated args to be numbers"]))))
    
    
    ; calculate-arith : ArithOp Number NEListOf<Number> -> Result<%>
    ; performs the arithmetic operation op on the 2ListOf<Number> (cons first nums)
    ; Strategy : Data decomposition on op: ArithOp
    (define (calculate-arith op first nums)
      (cond
        [(plus? op) (foldl (λ (b a) 
                             (new Number% 
                                  [ex (+ (send a to-expr) 
                                         (send b to-expr))])) first nums)]
        [(minus? op) (foldl (λ (b a)
                              (new Number%
                                   [ex (- (send a to-expr)
                                          (send b to-expr))])) first nums)]
        [(mult? op) (foldl (λ (b a)
                             (new Number%
                                  [ex (* (send a to-expr) 
                                         (send b to-expr))])) first nums)]
        [(div? op) (if (member? ZERO (map (λ (a) (send a to-expr)) nums))
                       (new ErrString% [ex "err: cannot divide by zero"])
                       (foldl (λ (a b) 
                                (new Number% 
                                     [ex (/ (send b to-expr) 
                                            (send a to-expr))])) first nums))]))
    
    
    ; eval-expr-ls : ListOf<Expr%> ListOf<Def> -> ListOf<Result<%>>
    ; Evaluates each element of ls to a Result in the context of the list of Defs.
    (define (eval-expr-ls ls defs)
      (map (λ (ex) (send ex eval defs)) ls))
        
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr which
    ; in this case is an arith
    (define/public (to-expr)
      (make-arith op args))
    
    
    ; to-expr/no-var/lst : ListOf<ListOf<UniqueParams>> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; by keeping  a track of List of parameters
    (define/public (to-expr/no-var/lst plist)
      (make-arith op
                  (map (λ (e) (send e to-expr/no-var/lst plist))
                       args)))
    
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))

    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given Expr<%>
    (define/public (expr=? e-to-cmp) 
      (equal? 
       (to-expr/no-var) 
       (send e-to-cmp to-expr/no-var)))
    
    (super-new)))



; A Lam% is a (new Lam% [params UniqueListOf<Param>] [body Expr%]
; Represents the Lam class that implements the Expr<%> interface
; INTERP: 
; params : Contains the parameters inthe lambda function
; body : Contains the expression corresponding to the lambda function
(define Lam% 
  (class* object% (Expr<%> Result<%>)
    (init-field params body)
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    (define/public (eval defs)
      (new Lam% [params params] [body body]))
    
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    (define/public (subst r x)
      (if (member? x params)
          this
          (new Lam% [params params] [body (send body subst r x)])))
    
    
    
    ; param-length-equal? : ListOf<Expr%> -> Maybe<ListOf<Result<%>>>
    ; Wrapper function that checks if the args provided
    ; are equal in length with the params of the lam object
    (define/public (param-length-equal? args)
      (= (length params) (length args)))
    
    
    (define/public (app args0 defs)
      (local (; subst-call/a : UniqueListOf<Param> ListOf<Result<%>> Expr% -> Expr%
              ; substitutes elements of params present in body with corresponding
              ; elements of args
              ; Where: (length params) = (length params). a is body with 
              ; substitutions made for Param from param0 not present in params
              ; Strategy: Data decomposition on params0 and args0: 
              ; UniqueListOf<Param> and ListOf<Result<%>>
              (define (subst-call/a paramsa argsa a)
                (cond
                  [(empty? argsa) a]
                  [else (subst-call/a 
                         (rest paramsa)
                         (rest argsa)
                         (send a subst (first argsa) (first paramsa)))])))
        ;--IN--
        (if (param-length-equal? args0)
            (subst-call/a params args0 body)
            (new ErrString% [ex "err: number of args don't match the number of params"]))))
    
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr which
    ; in this case is an arith
    (define/public (to-expr)
      (make-lam params  body ))
    
    
    ; to-expr/no-var/lst : ListOf<ListOf<UniqueParams>> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; by keeping  a track of List of parameters
    (define/public (to-expr/no-var/lst plist)
      (make-lam/no-var 
       (send body to-expr/no-var/lst
             (append params plist))))
    
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given Expr<%>
    (define/public (expr=? e-to-cmp) 
      (equal? 
       (to-expr/no-var) 
       (send e-to-cmp to-expr/no-var)))
    
    (super-new)))



; A Number% is a (new Number% [ex Natural]
; Represents the Number class that implements 
; the Expr<%> and Result<%> interface
; INTERP: 
; ex : Contains the natural number
(define Number% 
  (class* object% (Expr<%> Result<%>)
    (init-field ex)
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    ; Strategy : Function Composition
    (define/public (eval defs)
      this)
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    (define/public (subst r x)
      this)
    
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr which
    ; in this case is an arith
    (define/public (to-expr)
      ex)
    
    
    ; to-expr/no-var/lst : ListOf<ListOf<UniqueParams>> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; by keeping  a track of List of parameters
    (define/public (to-expr/no-var/lst plist)
      ex)
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))
    
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    ; If Result is a lambda, applies it to given arguments, else errors.
    (define/public (app args defs)
     args)
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given Expr<%>
    (define/public (expr=? e-to-cmp) 
      (equal? 
       (to-expr/no-var) 
       (send e-to-cmp to-expr/no-var)))
    
    (super-new)))


; A Boolean% is a (new Boolean% [ex Boolean]
; Represents the Boolean class that implements 
; the Expr<%> and Result<%> interface
; INTERP: 
; ex : Contains a boolean
(define Boolean% 
  (class* object% (Expr<%> Result<%>)
    (init-field ex)
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    (define/public (eval defs)
      this)
    
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    (define/public (subst r x)
      this)
    
    
    
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr which
    ; in this case is an arith
    (define/public (to-expr)
      ex)
    
    
    ; to-expr/no-var/lst : ListOf<ListOf<UniqueParams>> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; by keeping  a track of List of parameters
    (define/public (to-expr/no-var/lst plist)
      ex)
    
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))
    
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    ; If Result is a lambda, applies it to given arguments, else errors.
    (define/public (app args defs)
      args)
    
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given Expr<%>
    (define/public (expr=? e-to-cmp) 
      (equal? 
       (to-expr/no-var) 
       (send e-to-cmp to-expr/no-var)))
    
    (super-new)))


; A ErrString% is a (new ErrString% [ex String]
; Represents the ErrString class that implements 
; the Expr<%> and Result<%> interface
; INTERP: 
; ex : Contains a string
(define ErrString% 
  (class* object% (Expr<%> Result<%>)
    (init-field ex)
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    (define/public (eval defs)
      this)
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    (define/public (subst r x)
      this)
    
    
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr which
    ; in this case is an arith
    (define/public (to-expr)
      ex)
    
    
    ; to-expr/no-var/lst : ListOf<ListOf<UniqueParams>> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; by keeping  a track of List of parameters
    (define/public (to-expr/no-var/lst plist)
      ex)
    
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))
    
    
    ; app : ListOf<Result<%>> ListOf<Def> -> Result<%>
    ; If Result is a lambda, applies it to given arguments, else errors.
    (define/public (app args defs)
      args)
    
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given Expr<%>
    (define/public (expr=? e-to-cmp) 
      (equal? 
       (to-expr/no-var) 
       (send e-to-cmp to-expr/no-var)))
    
    (super-new)))


; A Var% is a (new Var% [ex Var]
; Represents the Var class that implements 
; the Expr<%> and Result<%> interface
; INTERP: 
; ex : Contains a Var that is the param name or function name
(define Var% 
  (class* object% (Expr<%>)
    (init-field ex)
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    (define/public (eval defs)
      (local (; gets the Def with the name var from the list of Defs
              (define df (get-def ex defs)))
        ;--IN--
        (if (equal? false df)
            (new ErrString% [ex "err: undefined var"])
            (eval-def df))))
    
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    (define/public (subst r x)
      (if (symbol=? x ex)
                  r
                  this))
    
    
    ; get-def : Var ListOf<Def> -> MayBe<Def>
    ; either returns the Def with the name var or returns false if there is no Def
    ; with the name var in defs
    ; Strategy: Data decomposition on defs: ListOf<Def> 
    (define (get-def var defs)
      (cond
        [(empty? defs) false]
        [else (if (is-def-name? var (first defs))
                  (first defs)
                  (get-def var (rest defs)))]))
    
    
    ; eval-def : Def -> Lambda
    ; converts the Def d to a Lambda
    ; Strategy: Data decomposition on d: Def
    (define (eval-def d)
      (new Lam% [params (def-params d)] [body (def-body d)]))
    
    
    ; is-def-name? : Var Def -> Boolean
    ; checks if the name of the Def d is var
    ; Strategy: Data decomposition on d: Def
    (define (is-def-name? var d)
      (symbol=? var (def-name d)))
    
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr which
    ; in this case is an arith
    (define/public (to-expr)
      ex)
    
    
    ; to-expr/no-var/lst : ListOf<ListOf<UniqueParams>> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; by keeping  a track of List of parameters
    (define/public (to-expr/no-var/lst plist)
      (get-static-d plist))
    
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))

    
    
    ;; get-static-d : ListOf<UniqueListOf<Param>> -> StaticDist
    ;; returns the static distance of ex var given the list of 
    ;; params of all the lamdas found till ex var was encountered.
    ;; Where: plist0 is in the reverse order of occurance
    (define (get-static-d plist0)
      (list (get-first-depth plist0) 
            (get-first-index plist0)))
    
    
    ;; get-first-index : ListOf<UniqueListOf<Param>> -> Natural
    ;; returns the ''index of var'' in the first UniqueListOf<Param> it is found in
    ;; plist0
    ;; Where: var is present in plist0
    (define (get-first-index plist0)
      (local (; get-first-index/a: ListOf<UniqueListOf<Param>> Natural Boolean
              ; returns the ''index of var'' in the first UniqueListOf<Param> it is
              ; found in plist
              ; Where: a is the index of var ex in the first list it is found in and
              ; found? keeps track of whether var ex index was found or not
              ; Strategy: Data decomposition on plist: ListOf<UniqueListOf<Param>>
              (define (get-first-index/a plist a found?)
                (cond
                  [(empty? plist) a]
                  [else (if (and (member? ex (list (send (first plist) to-expr))) (not found?))
                            (get-first-index/a (rest plist) 
                                               (get-index ex (list (send (first plist) to-expr)))
                                               true)
                            (get-first-index/a (rest plist) a found?))])))
        ;--IN--
        (get-first-index/a  plist0 ZERO false)))
    
    ;; get-first-depth : ListOf<UniqueListOf<Param>> -> Natural
    ;; returns the ''index of the first UniqueListOf<Param>'' in which var is found 
    ;; in plist0
    ;; Where: var is present in plist0
    (define (get-first-depth plist0)
      (local (; get-first-depth/a: ListOf<UniqueListOf<Param>> Natural Boolean
              ; returns the index of the first UniqueListOf<Param> in which var ex is 
              ; found in plist
              ; Where: a is the number of lists in plist0 that are not in plist and
              ; not-found? keeps track of whether var ex was found or not
              ; Strategy: Data decomposition on plist: ListOf<UniqueListOf<Param>>
              (define (get-first-depth/a plist a not-found?)
                (cond
                  [(empty? plist) a]
                  [else (if (and (not 
                                  (member? 
                                   ex (list 
                                       (send 
                                        (first plist)
                                        to-expr)))) not-found?)
                        (get-first-depth/a (rest plist) (add1 a) not-found?)
                        (get-first-depth/a (rest plist) a false))])))
    ;--IN--
        (get-first-depth/a plist0 ZERO true)))
    
    
    ;; get-index : Var UniqueListOf<Param> -> Natural
    ;; returns the index of item in ls0
    ;; Where: item is present in ls0
    (define (get-index item ls0)
      (local (; get-index/a : UniqueListOf<Param> Natural Boolean
              ; returns the index of item
              ; Where: a is the number of items in ls0 not in ls if item has not 
              ; been found yet. not-found? keeps track of whether or not item was 
              ; found. 
              ; Strategy: Data decomposition on ls0: UniqueListOf<Param>
              (define (get-index/a ls a not-found?)
                (cond
                  [(empty? ls) a]
                  [else (if (and not-found? (not (symbol=? (first ls) item)))
                        (get-index/a (rest ls) (add1 a) true)
                        (get-index/a (rest ls) a false))])))
        ;--IN--
        (get-index/a ls0 ZERO true)))
    
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given Expr<%>
    (define/public (expr=? e-to-cmp) 
      (equal? 
       (to-expr/no-var) 
       (send e-to-cmp to-expr/no-var)))
    
    (super-new)))



; A Bool% is a (new Bool% [op BoolOp] [Args 2ListOf<Expr%>]
; Represents the Bool class that implements the Expr<%> interface
; INTERP: 
; op : Contains boolean operator and, or
; args : Contains a 2ListOf<Expr%> which can further be simplified and
; finally evaluates the given boolean expression
(define Bool% 
  (class* object% (Expr<%>)
  (init-field op args)
    
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    (define/public (eval defs)
      (eval-bool op (eval-expr-ls args defs)))
    
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    (define/public (subst r x)
      (new Bool% [op op] 
                [args (map (λ (ex) (send ex subst r x)) args)]))
    
    
    ; eval-bool : BoolOp 2ListOf<Result<%>> -> Result<%>
    ; Performs the boolean operation op on args, if all elements of args are 
    ; numbers
    ; Strategy: Data decomposition on args: 2ListOf<Result<%>>
    (define (eval-bool op args)
      (if (andmap (λ (a) (boolean? (send a to-expr))) args)
          (new Boolean% [ex (calculate-bool op args)])
          (new ErrString% [ex "err: BoolExpr expects evaluated args to be booleans"])))
    
    
    ; eval-expr-ls : ListOf<Expr%> ListOf<Def> -> ListOf<Result<%>>
    ; Evaluates each element of ls to a Result in the context of the list of Defs.    
    (define (eval-expr-ls ls defs)
      (map (λ (ex) (send ex eval defs)) ls))
    
    
    ; calculate-bool : BoolOp NEListOf<Boolean> -> Result
    ; performs the boolean operation op on the 2ListOf<Boolean> (cons first nums)
    ; Strategy : Data decomposition on op: BoolOp
    (define (calculate-bool op args)
      (cond
        [(and? op) (foldr (λ (a b) (and (send a to-expr) b)) TRUE args)]
        [(or? op) (foldr (λ (a b) (or (send a to-expr) b)) FALSE args)]))
        
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr which
    ; in this case is an arith
    (define/public (to-expr)
      (make-bool op args))
    
    
    ; to-expr/no-var/lst : ListOf<ListOf<UniqueParams>> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; by keeping  a track of List of parameters
    (define/public (to-expr/no-var/lst plist)
      (make-bool op 
                 (map (λ (e) (send e to-expr/no-var/lst plist))
                      args)))
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))

    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given Expr<%>
    (define/public (expr=? e-to-cmp) 
      (equal? 
       (to-expr/no-var) 
       (send e-to-cmp to-expr/no-var)))
    
    (super-new)))

(define Cmp% 
  (class* object% (Expr<%>)
  (init-field op args)
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    (define/public (eval defs)
      (eval-cmp op (eval-expr-ls args defs)))
    
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    (define/public (subst r x)
      (new Cmp% [op op] 
                [args (map (λ (ex) (send ex subst r x)) args)]))
    
    ; eval-expr-ls : ListOf<Expr%> ListOf<Def> -> ListOf<Result<%>>
    ; Evaluates each element of ls to a Result in the context of the list of Defs.
    (define (eval-expr-ls ls defs)
      (map (λ (ex) (send ex eval defs)) ls))
    
    
    ; eval-cmp : CmpOp 2ListOf<Result<%>> -> Result<%>
    ; Performs the comparison operation op on args,
    ; if all elements of args are numbers    
    (define (eval-cmp op args)
      (if (andmap (λ (a) (number? 
                               (send a to-expr))) args)
          (new Boolean% [ex (calculate-cmp op (first args) (rest args))])
          (new ErrString% [ex "err: CmpExpr expects evaluated args to be numbers"])))
    
    ; calculate-cmp : CmpOp Number NEListOf<Number> -> Result
    ; performs the arithmetic operation op on the 2ListOf<Number> (cons first nums)
    ; Strategy : Data decomposition on op: CmpOp
    (define (calculate-cmp op first args)
      (local (; full list of arguments
              (define all (cons 
                           (send first to-expr) 
                           (map (λ (a) (send a to-expr)) args))))
        ;--IN--
        (cond
          [(comp-eq? op) (andmap 
                          (λ (a) 
                            (= (send a to-expr) (send first to-expr))) args)]
          [(comp<? op) (and 
                        (not 
                         (not-unique? args)) 
                        (equal? (sort all <) all))]
          [(comp>? op) (and 
                        (not 
                         (not-unique? args)) 
                        (equal? (sort all >) all))])))
    
    ; not-unique? : ListOf<Number> -> Boolean
    ; returns true if any two numbers in ls are equal
    ; Strategy: Data decomposition on ls: ListOf<Number>
    (define (not-unique? ls)
      (cond
        [(empty? ls) FALSE]
        [else (if (ormap (λ (n) (= (send n to-expr) (send (first ls) to-expr))) (rest ls))
                  TRUE
                  (not-unique? (rest ls)))]))
        
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr which
    ; in this case is an arith
    (define/public (to-expr)
      (make-cmp op args))
    
    
    ; to-expr/no-var/lst : ListOf<ListOf<UniqueParams>> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; by keeping  a track of List of parameters
    (define/public (to-expr/no-var/lst plist)
      (make-cmp op 
                 (map (λ (e) (send e to-expr/no-var/lst plist))
                      args)))
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))

    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given Expr<%>
    (define/public (expr=? e-to-cmp) 
      (equal? 
       (to-expr/no-var) 
       (send e-to-cmp to-expr/no-var)))
    
    (super-new)))

; A If-Exp% is a (new If-Exp% [test Expr%] [branch1 Expr%] [branch2 Expr%]
; Represents the If-Exp class that implements the Expr<%> interface
; INTERP: 
; test : test condition of the if
; branch1 : value of the expression if the test is true
; branch2 : value of the expression if the test is false
(define If-Exp% 
  (class* object% (Expr<%>)
  (init-field test branch1 branch2)
    
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    (define/public (eval defs)
      (send (get-if-branch (send test eval defs)
                           branch1
                           branch2)
            eval
            defs))
    
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    (define/public (subst r x)
      (new If-Exp% [test (send test subst r x)]
                  [branch1 (send branch1 subst r x)]
                  [branch2 (send branch2 subst r x)]))
    
    
    ; get-if-branch : Result<%> Result<%> Result<%> -> Result<%>
    ; the if condition is evaluated if test is a boolean.
    (define (get-if-branch test b1 b2)
      (if (boolean? (send test to-expr))
          (if (send test to-expr) b1 b2)
          (new ErrString% [ex "err: test in If-Exp should evaluate to a boolean"])))
    
    
    ; eval-expr-ls : ListOf<Expr%> ListOf<Def> -> ListOf<Result<%>>
    ; Evaluates each element of ls to a Result in the context of the list of Defs.
    (define (eval-expr-ls ls defs)
      (map (λ (ex) (send ex eval defs)) ls))    
    
    
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr which
    ; in this case is an arith
    (define/public (to-expr)
      (make-if-exp test
                  branch1 
                  branch2))
    
    
    ; to-expr/no-var/lst : ListOf<ListOf<UniqueParams>> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; by keeping  a track of List of parameters
    (define/public (to-expr/no-var/lst plist)
      (make-if-exp (send test to-expr/no-var/lst plist)
                   (send branch1 to-expr/no-var/lst (send plist to-expr))
                   (send branch2 to-expr/no-var/lst (send plist to-expr))))
    
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))
    
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given Expr<%>
    (define/public (expr=? e-to-cmp) 
      (equal? 
       (to-expr/no-var) 
       (send e-to-cmp to-expr/no-var)))
    
    (super-new)))

; A Call% is a (new Call% [fn Var] [args ListOf<Expr%>]
; Represents the Call class that implements the Expr<%> interface
; INTERP: 
; fn : name of the funtion that has to be called
; args : argument names that are being passed to the function
(define Call% 
  (class* object% (Expr<%>)
  (init-field fn args)
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    (define/public (eval defs)
      (send (try-subst-call (send fn eval defs) 
                            (eval-expr-ls args defs) defs)
            eval
            defs))
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    (define/public (subst r x)
      (new Call% [fn (send fn subst r x)] 
                [args (map (λ (ex) (send ex subst r x)) args)]))
    
    
    ; try-subst-call : Expr ListOf<Result<%>> ListOf<Def> -> Result<%>
    ; if fn is lambda, the body of lambda is returned after substituting params of 
    ; fn with corresponding elements of args, else an ErrString is returned
    (define (try-subst-call fn args defs)
      (if (lambda? (send fn to-expr))
          (send fn app args defs)
          (send (new ErrString% [ex "err: function call sub expression didn't evaluate to lambda"]) app fn args)))
    
    
    ; eval-expr-ls : ListOf<Expr%> ListOf<Def> -> ListOf<Result<%>>
    ; Evaluates each element of ls to a Result in the context of the list of Defs.
    (define (eval-expr-ls ls defs)
      (map (λ (ex) (send ex eval defs)) ls))
    
    
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr which
    ; in this case is an arith
    (define/public (to-expr)
      (make-call fn args))
    
    
    ; to-expr/no-var/lst : ListOf<ListOf<UniqueParams>> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; by keeping  a track of List of parameters
    (define/public (to-expr/no-var/lst plist)
      (make-call (send fn to-expr/no-var/lst plist)
                 (map (λ (e) (send e to-expr/no-var/lst plist))
                      args)))
    
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))
    
    
    ; expr=? : Expr<%> -> Boolean
    ; Returns true if this expression is structurally equivalent to the given Expr<%>
    (define/public (expr=? e-to-cmp) 
      (equal? 
       (to-expr/no-var) 
       (send e-to-cmp to-expr/no-var)))
    
    (super-new)))

; A Program% is a (new Program% [prg List<Program>]
; Represents the Program class that implements the Program<%> interface
; INTERP: 
; prg : Represents the list of Expr<%> and Program<%> or list of Def and Program<%>
(define Program% 
  (class* object% (Program<%>)
    (init-field prg)
    
    ; eval : -> ListOf<Result<%>>
    ; Evaluates expressions in the program to Result<%>s
    ; WHERE: A function may be called before it is defined.
    ; WHERE: The results have the same order as their original expressions in the program.
    (define/public (eval)
      (eval-with-defns (gather-defns)))
    
    
    ; eval-with-defns : ListOf<Def> -> ListOf<Result<%>>
    ; Evaluates expressions in the program to Result<%>s with the list of all the defs in
    ; the program
    ; WHERE: A function may be called before it is defined.
    ; WHERE: The results have the same order as their original expressions in the program.
    
    (define/public (eval-with-defns defs)
      (local (
          ; eval-local : Program% -> ListOf<Result<%>>
          ; Given a PDPLang program and a list of all Defs in the program, 
          ; evaluates the PDPLang program into a list of Results.
          ; Strategy: Data decomposition on p: Program
          (define (eval-local p)
            (cond
              [(empty? p) MT-LIST]
              [(not (def? (first p))) (cons (send (first p) eval defs)
                                            (send (second p) eval-with-defns defs))]
              [else (send (second p) eval-with-defns defs)])))
    ;--IN--
    (eval-local prg)))
       
      ; gather-defns : -> ListOf<Def>
      ; Gets all the list of def in the given program prgm0
      (define/public (gather-defns)
        (local (;; gather-defns/a : Program Listof<Def> -> Listof<Def>
                ;; Returns all the def entities present in the program as a list
                ;; WHERE: def-so-far is the list of defs that is being computed from 
                ;; prgm0
                ;; Strategy : Data decomposiiton on prgm : Program
                (define (gather-defns/a prgm defs-so-far)
                  (cond
                    [(empty? prgm) defs-so-far]
                    [(def? (first prgm))
                     (gather-defns/a (send (second prgm) gather-defns) (cons (first prgm) defs-so-far))]
                    [else
               (gather-defns/a (send (second prgm) gather-defns) defs-so-far)])))
          ;; - IN -
          (gather-defns/a prg '())))
    
    (super-new)))


; mk-Program% : Program -> Program<%>
; Creates the Program class from the given program itemization
; Strategy : Data decomposition on p : Program
(define (mk-Program% p)
  (cond
    [(empty? p) (new Program% [prg MT-LIST])]
    [(not (def? (first p))) (new Program% [prg (list (mk-Expr% (first p)) (mk-Program% (rest p)))])]
    [else (new Program% [prg (list (mk-def (first p)) (mk-Program% (rest p)))])]))


; mk-def : Def -> Def
; Converts body part of the def to Expr<%>
; Strategy : Data decomposition on df : Def
(define (mk-def df)
  (make-def (def-name df) 
           (def-params df)
           (mk-Expr% (def-body df))))


; mk-Expr% : Expr -> Expr<%>
; Creates the Expr class from the given Expr itemization
; Strategy : Data decomposition on ex : Expr
(define (mk-Expr% ex)
  (cond
        [(number? ex) (new Number% [ex ex])]
        [(boolean? ex) (new Boolean% [ex ex])]
        [(var? ex) (new Var% [ex ex])]
        [(errstr? ex) (new ErrString% [ex ex])]
        [(lambda? ex) (new Lam% [params (mk-Expr%-ls (lam-params ex))] [body (mk-Expr% (lam-body ex))])]
        [(arith? ex) (new Arith% [op (arith-op ex)] [args (mk-Expr%-ls (arith-args ex))])]
        [(bool? ex) (new Bool% [op (bool-op ex)] [args (mk-Expr%-ls (bool-args ex))])]
        [(cmp? ex) (new Cmp% [op (cmp-op ex)] [args (mk-Expr%-ls (cmp-args ex))])]
        [(if-exp? ex) (new If-Exp% [test (mk-Expr% (if-exp-test ex))] 
                           [branch1 (mk-Expr% (if-exp-branch1 ex))] 
                           [branch2 (mk-Expr% (if-exp-branch2 ex))])]
        [(call? ex) (new Call% [fn (mk-Expr% (call-fn ex))] [args (mk-Expr%-ls (call-args ex))])]))


; mk-Expr%-ls : ListOf<Expr> -> ListOf<Expr%>
; Evaluates each element of ls to a Expr%
; Strategy: Function Composition
(define (mk-Expr%-ls ls)
  (map (λ (ex) (mk-Expr% ex)) ls))


; member? : X ListOf<X> -> Boolean
; Checks if the given X exists in the ListOf<X>
; Strategy: Function Composition
(define (member? item list)
  (ormap (λ (ls-item)
           (eq? item ls-item)) list))

(define PGM-MIXED (list (make-def 'testfn (list 'x 'y)
                    (make-arith '- (list (make-arith '* (list 'x 'y))
                                         (make-arith '+ (list 'x 'y)))))
          (make-call 'testfn (list 10 11))))


 ;; Tests
;;------------------------------------------------------------------------------

;; Tests for eval
(begin-for-test
  (check-equal?  (send (mk-Program% '()) eval)  '())
  (check-equal?  (send (first (send (mk-Program% PGM-MIXED) eval)) to-expr)
                 89
                 "mixed evaluation")
  (check-true (send (first (send (mk-Program% 
                                  (list (make-def 'testfn (list 'x 'y 'z)
                                                  (make-cmp '> (list 'x 'y 'z)))
                                        (make-call 'testfn (list 13 12 11))))
                                 eval)) to-expr)
              "CMP")
  (check-equal? (send (first (send (mk-Program% 
                                    (list 
                                     (make-def 'testfn (list 'x 'y 'z)
                                               (make-arith '+ (list 'x 'y 'z)))
                                     (make-call 'testfn (list 13 12 11))))
                                   eval)) to-expr)
                36
                "arith")
  (check-equal? (send (first (send (mk-Program% 
                                    (list 
                                     (make-def 'testfn (list 'x 'y )
                                               (make-arith '/ (list 'x 'y )))
                                     (make-call 'testfn (list 24 12 ))))
                                   eval)) to-expr)
                2
                "div")
  (check-equal? (send (first (send (mk-Program% 
                                    (list 
                                     (make-def 'testfn (list 'x 'y 'z)
                                               (make-arith '/ (list 'x 'y 'z )))
                                     (make-call 'testfn (list 24 2 0))))
                                   eval)) to-expr)
                "err: cannot divide by zero"
                "div by zero")
  (check-equal? (send 
                 (send 
                  (mk-Expr% (make-arith '+ (list 1 2 3)) )eval '()) to-expr)
                6
                "arith")
  (check-equal? (send 
                 (send 
                  (mk-Expr% (make-arith '+ (list "a" '2 3)) )eval '()) to-expr)
                "err: ArithExpr expects evaluated args to be numbers"
                "Error")
  (check-equal?  (send 
                  (send
                   (mk-Expr%
                    (make-bool
                     'and (list
                           (make-bool 'or (list true true false))
                           true false)))eval '()) to-expr)
                 #f "boolean")
  (check-equal? (send (send (mk-Expr% 
                             (make-cmp '= (list 1 1 1))) eval '())to-expr)
                true
                "equal")
  (check-equal? (send (send (mk-Expr% 
                             (make-cmp '> (list 1 2 3))) eval '())to-expr)
                false
                "not greater than")
  (check-equal? (send (send (mk-Expr%
                             (make-cmp '> (list 31 21 1))) eval '())to-expr)
                true
                "greater than")
  (check-equal? (send (send (mk-Expr% 
                             (make-cmp '< (list 21 11 1))) eval '())to-expr)
                false
                "not-less than")
  (check-equal? (send (send (mk-Expr% 
                             (make-cmp '< (list 1 12 13))) eval '())to-expr)
                true
                "less than")
  (check-equal? (send (send 
                       (mk-Expr% 
                        (make-if-exp (make-cmp '< (list 1 2 0))
                                     (make-arith '* (list 1 2 3))
                                     (make-arith '* (list 1 2 1))))
                       eval '()) to-expr)
                2
                "second branch evaluated")
  (check-equal? (send (send 
                       (mk-Expr% 
                        (make-if-exp (make-cmp '> (list 1 0))
                                     (make-arith '* (list 1 2 3))
                                     (make-arith '* (list 1 2 1))))
                       eval '()) to-expr)
                6
                "second branch evaluated")
  (check-equal? (send (send 
                       (mk-Expr% 
                        (make-if-exp (make-cmp '- (list 1 0))
                                     (make-arith '* (list 1 2 3))
                                     (make-arith '* (list 1 2 1))))
                       eval '()) to-expr)
                "err: test in If-Exp should evaluate to a boolean"
                "error evaluated")
  (check-equal? (send (first 
                       (send 
                        (mk-Program% 
                         (list 
                          (make-def 
                           'testfn (list 'x 'y 'z)
                           (make-arith '- 
                                       (list (make-arith '* 
                                                         (list 'x 'y 'z))
                                             (make-arith '+ (list 'x 'y 'z)))))
                          (make-call 'testfn (list 10 11 12)))) eval)) to-expr)
                
                1287
                "program for arith")
  (check-equal? (send (mk-Expr% 
                       (make-lam '(x y) 
                                 (make-lam '(z) 
                                           (make-call 'x '(y z))))) 
                      to-expr/no-var)
                (lam/no-var (lam/no-var (call '(1 0) '((2 0) (0 0)))))
                "lambda evaluated without variable")
  
  (check-true
   (send 
    (mk-Expr% (make-lam '(y) (make-lam '(x) (make-call 'y '(x)))))
       expr=? (mk-Expr% (make-lam '(x) (make-lam '(y) (make-call 'x '(y)))))) 
      "equivalent nested-lambdas")
     
     (check-true  
      (send (mk-Expr% 
             (make-cmp '< (list 21 11 1)))
            expr=? (mk-Expr% (make-cmp '< (list 21 11 1))))
      "equal expr")
     
     (check-true (send (mk-Expr% 
                        (make-arith '+ (list 21 11 1)))
                       expr=? (mk-Expr% (make-arith '+ (list 21 11 1))))
                 
                 "equal expr")
     (check-false  (send (mk-Expr% 
                          (make-arith '+ (list 21 11 1)))
                         expr=? (mk-Expr% (make-arith '+ (list  11 1 21))))
                   
                   "not equal expr")
     (check-true (send (mk-Expr% 
                        (make-bool 'and (list true true))) 
                       expr=? (mk-Expr% (make-bool 'and (list true true))))
                 
                 "equal expr")
    (check-true (send (mk-Expr% 
                         true)  
                       expr=? (mk-Expr% true))
                "equal expr")
    (check-true (send (mk-Expr% 
                         2)  
                       expr=? (mk-Expr% 2))
                "equal expr")
    (check-true (send (mk-Expr% 
                         "err:error")  
                       expr=? (mk-Expr% "err:error"))
                "equal expr")
                 
(check-true (send (mk-Expr% 
                         (make-call 'testfn (list 'x 'y)))  
                       expr=? (mk-Expr% (make-call 'testfn (list 'x 'y))))
                "equal expr")

(check-false 
 (send (first 
        (send 
         (mk-Program% 
          (list (make-def 'testfn (list 'x 'y 'z 'a 'b 'c)    
                          (make-if-exp 
                           (make-cmp
                            '< 
                            (list (make-arith '* (list 'x 'y 'z))
                                  (make-arith '+ (list 'a 'b 'c)))) 
                           (make-bool 'and (list true false)) 
                           (make-bool 'or (list true false))))
                
                
                (make-call 'testfn (list 1 2 3 4 5 6)))) eval))to-expr)
 
 "program is evaluated to false")
(check-equal?  
 (send (first 
        (send 
         (mk-Program% 
          (list (make-def 'testfn (list 'x 'y 'z 'a 'b 'c)    
                          (make-if-exp 
                           (make-cmp
                            '< 
                            (list (make-arith '* (list 'x 'y 'z))
                                  (make-arith '+ (list 'a 'b 'c)))) 
                           (make-bool 'and (list true false)) 
                           (make-bool 'or (list true false))))
                
                
                (make-call 'tes3tfn (list 1 2 3 4 5 6)))) eval))to-expr)
 
 "err: undefined var"
 "invalid lambda")
(check-true (call?
  (send (mk-Expr% (make-call 'testfn (list 'x 'y)))  
                       to-expr))

"call function")
(check-true (arith?
  (send (mk-Expr%  (make-arith '+ (list 1 1)))  
                       to-expr))

"arith function")
(check-true (lam?
             (send (send (mk-Expr% (make-lam '(x y) (make-lam '(z) 
                           (make-call 'x '(y z))))) eval '()) to-expr))
            "lam function")
(check-true (lam?
             (send (send (mk-Expr% (make-lam '(x y) (make-lam '(z) 
                           (make-call 'x '(y z))))) subst 4 'x)to-expr))
            "lam subst function")
(check-equal?
 (send (mk-Expr% 'x) subst 4 'x)
 4
 "subst"))

