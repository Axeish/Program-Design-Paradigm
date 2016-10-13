;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname eval) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Required Libraries
;;-----------------------------------------------------------------------------
(require "extras.rkt")
(require rackunit)
(define TIME-ON-TASK 14)


;; Provide Functions
;;-----------------------------------------------------------------------------
(provide eval)
(provide lambda?)
(provide errstr?)
(provide subst)
(provide expr->expr/no-var)
(provide expr=?)

;; Constants
;;-----------------------------------------------------------------------------
(define ZERO 0)
(define MT-LIST '())
(define TRUE true)
(define FALSE false)

;; Data Definitions
;;------------------------------------------------------------------------------

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
;;     ; (second ls) left out to ensure at least 2 elements are passed into 
;;     ; 2ListOf-fn
;;    [else (...(first ls)...(2ListOf-fn (rest ls))...)]))

;;------------------------------------------------------------------------------

;; A Def is a (make-def FnName UniqueListOf<Param> Expr)
;; Represents the definition of a function with the specified parameters.
(define-struct def (name params body))

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
(define-struct lam (params body))

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
(define-struct arith (op args))
(define-struct bool (op args))
(define-struct cmp (op args))
(define-struct if-exp (test branch1 branch2))
(define-struct call (fn args))

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
;;     [(lambda? ex) (...(param-ls-fn (lam-params ex))...
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

;;------------------------------------------------------------------------------

;; An ArithOp is one of:
;; - '+
;; - '-
;; - '*
;; - '/
;; Represents an arithmetic operation in PDPLang

;; ArithOp predicates> : ArithOp -> Boolean
;; Returns true if op is the ArithOp indicated by the function name.
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
;;     [(lambda? r) (...(param-ls-fn (lam-params r))...
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
;; Represents a Lambda with only the body with parameters in the body replaced 
;; with its StaticDist
(define-struct lam/no-var (body))

;; Example
(define LAM-NO-VAR (make-lam/no-var SD1))

;; Template
(define (lamNoVar-fn lm)
  (...(expr/no-var-fn (lam/no-var-body lm))...))

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

;; Constants
(define MK-ADD-DEF (make-def 'mk-add '(n) (make-lam '(m) 
                                                    (make-arith '+ '(n m)))))
(define ADD5-DEF (make-def 'add5 '(x) (make-call (make-call 'mk-add '(5)) 
                                                 '(x))))
; add5-or-6 : adds 5 to y if y it's positive, else adds 6
(define ADD5-OR-6-DEF (make-def 'add5-or-6 '(y)
                                (make-if-exp (make-cmp '> '(y 0))
                                             (make-call 'add5 '(y))
                                             (make-call (make-call 'mk-add '(6))
                                                        '(y)))))
;; Functions
;;------------------------------------------------------------------------------

;; eval : Program -> ListOf<Result>
;; Evaluates a PDPLang program to a list of Results.
;; Specifically, evaluates the Exprs in p, in the context of the given Defs.
;; WHERE: A function may be called before it is defined.
;; WHERE: The produced results are in the same relative order as their 
;; originating Exprs in p.

;; Examples

(begin-for-test
  (check-equal? (first (eval (list (make-arith '+ (list 1 2 3)))))
                6
                "addition")
  (check-pred lambda? (first (eval (list DEF 'f))) "result is lambda")
  (check-pred errstr? (first (eval (list DEF 'g))) "err: undefined var"))

;; Strategy : Function composition


(define (eval p0)
  (local (; all the Defs in the program
          (define defs (filter def? p0))
          ; eval-local : Program -> ListOf<Result>
          ; Given a PDPLang program and a list of all Defs in the program, 
          ; evaluates the PDPLang program into a list of Results.
          ; Strategy: Data decomposition on p: Program
          (define (eval-local p)
            (cond
              [(empty? p) MT-LIST]
              [(not (def? (first p))) (cons (eval-expr (first p) defs)
                                            (eval-local (rest p)))]
              [else (eval-local (rest p))])))
    ;--IN--
    (eval-local p0)))

;;------------------------------------------------------------------------------

;; eval-expr-ls : ListOf<Expr> ListOf<Def> -> ListOf<Result>
;; Evaluates each element of ls to a Result in the context of the list of Defs.

;; Examples:

(begin-for-test
  (check-equal? (eval-expr-ls (list 24 true (make-arith '- (list 2 1))) '())
                (list 24 true 1)
                "evaluates all the exprs"))

;; Strategy: Function composition

(define (eval-expr-ls ls defs)
  (map (λ (ex) (eval-expr ex defs)) ls))

;;------------------------------------------------------------------------------

;; eval-expr : Expr ListOf<Def> -> Result
;; Evaluates ex to a Result in the context of the list of Defs.

;; Examples

(begin-for-test
  (check-equal? (eval-expr 24 '()) 24)
  (check-equal? (eval-expr 'f (list (make-def 'f '(a) (make-arith '+ '(a 2)))))
                (make-lam '(a) (make-arith '+ '(a 2))))
  (check-equal? (eval-expr 'func '()) "err: undefined var")
  (check-equal? "error" "error"))

;; Strategy: Generative Recursion
;; TERMINATION ARGUMENT: Does not terminate when make-call is called recursively
;; example:
;; (define F-RECUR (make-def 'fn '(x) (make-call 'fn (list 'x))))
;; (eval (list F-RECUR (make-call 'fn (list 2))))

(define (eval-expr ex defs)
  (cond
    [(number? ex) ex]
    [(boolean? ex) ex]
    [(var? ex) (eval-var ex defs)]
    [(errstr? ex) ex]
    [(lambda? ex) ex]
    [(arith? ex) (eval-arith (arith-op ex) (eval-expr-ls (arith-args ex) defs))]
    [(bool? ex) (eval-bool (bool-op ex) (eval-expr-ls (bool-args ex) defs))]
    [(cmp? ex) (eval-cmp (cmp-op ex) (eval-expr-ls (cmp-args ex) defs))]
    [(if-exp? ex) (eval-expr (get-if-branch (eval-expr (if-exp-test ex) defs)
                                            (if-exp-branch1 ex)
                                            (if-exp-branch2 ex))
                             defs)]
    [(call? ex) (eval-expr (try-subst-call (eval-expr (call-fn ex) defs) 
                                           (eval-expr-ls (call-args ex) defs))
                           defs)]))

;;------------------------------------------------------------------------------

;; eval-var : Var ListOf<Def> -> Result
;; var evaluates to a lambda if there is a Def in defs with the name Var, else 
;; it evaluates to an ErrString

;; Examples

(begin-for-test
  (check-equal? (eval-var 'f (list (make-def 'f '(a) (make-arith '+ '(a 2)))))
                (make-lam '(a) (make-arith '+ '(a 2)))
                "var 'f evaluates to the equivalent lambda")
  (check-equal? (eval-var 'func '()) 
                "err: undefined var"))

;; Strategy : Function composition

(define (eval-var var defs)
  (local (; gets the Def with the name var from the list of Defs
          (define df (get-def var defs)))
    ;--IN--
    (if (equal? false (get-def var defs))
        "err: undefined var"
        (get-lam-from-def df))))

;;------------------------------------------------------------------------------

;; calculate-arith : ArithOp Number NEListOf<Number> -> Result
;; performs the arithmetic operation op on the 2ListOf<Number> (cons frst rst)
;; returns an ErrString for divide by zero

;; Examples

(begin-for-test
  (check-equal? (calculate-arith '- 8 '(3 1 4))
                0
                "performs the arithmetic operation")
  (check-equal? (calculate-arith '/ 16 '(1 2 2 2)) 
                2
                "performs the arithmetic operation")
  (check-equal? (calculate-arith '/ 16 '(2 0)) 
                "err: cannot divide by zero"
                "can not divide by 0"))

;; Strategy : Data decomposition on op: ArithOp

(define (calculate-arith op frst rst)
  (local (; combines frst and rst into a single list
          (define args (cons frst rst)))
  (cond
    [(plus? op) (apply + args)]
    [(minus? op) (apply - args)]
    [(mult? op) (apply * args)]
    [(div? op) (if (member? ZERO rst)
                   "err: cannot divide by zero"
                   (apply / args))])))

;;------------------------------------------------------------------------------

;; eval-arith : ArithOp 2ListOf<Result> -> Result
;; Performs the arithmetic operation op on args, if all elements of args are 
;; numbers, else returns an ErrString

;; Examples

(begin-for-test
  (check-equal? (eval-arith '+ (list 8 3 1 4))
                16
                "performs the arithmetic operation"))

;; Strategy: Data decomposition on args: 2ListOf<Result>

(define (eval-arith op args)
  (if (andmap number? args)
        (calculate-arith op (first args) (rest args))
        "err: ArithExpr expects evaluated args to be numbers"))

;;------------------------------------------------------------------------------

;; calculate-bool : BoolOp 2ListOf<Boolean> -> Boolean
;; Performs the boolean operation op on args
;; booleans

;; Examples

(begin-for-test
  (check-equal? (calculate-bool 'and (list true true false))
                false
                "performs the boolean operation")
  (check-equal? (calculate-bool 'or (list true false false false)) 
                true
                "performs the boolean operation"))

;; Strategy : Data decomposition on op: BoolOp

(define (calculate-bool op args)
  (cond
    [(and? op) (andmap (λ (a) a)args)]
    [(or? op) (ormap (λ (a) a)args)]))

;;------------------------------------------------------------------------------

;; eval-bool : BoolOp 2ListOf<Result> -> Result
;; Performs the boolean operation op on args, if all the elements of args are 
;; boolean, else returns an ErrString

;; Examples

(begin-for-test
  (check-equal? (eval-bool 'and (list true true false))
                false
                "performs the boolean operation")
  (check-equal? (eval-bool 'or (list 12 false false false)) 
                "err: BoolExpr expects evaluated args to be booleans"
                "wrong kind of arguments"))

;; Strategy: Function composition

(define (eval-bool op args)
  (if (andmap boolean? args)
        (calculate-bool op args)
        "err: BoolExpr expects evaluated args to be booleans"))


;;------------------------------------------------------------------------------

;; calculate-cmp : CmpOp 2ListOf<Number> -> Boolean
;; performs the comparison operation op on list args

;; Examples

(begin-for-test
  (check-equal? (eval-cmp '= (list 1 1 1))
                true
                "all numbers are equal")
  (check-equal? (eval-cmp '> (list 1 1 1)) 
                false
                "not greater than"))

;; Strategy: Data decomposition on op: CmpOp

(define (calculate-cmp op args)
  (cond
      [(comp-eq? op) (apply = args)]
      [(comp<? op) (apply < args)]
      [(comp>? op) (apply > args)]))

;;------------------------------------------------------------------------------

;; eval-cmp : CmpOp 2ListOf<Result> -> Result
;; Performs the boolean op on args, if all the elements of args are numbers, 
;; else returns an ErrString

;; Examples

(begin-for-test
  (check-equal? (eval-cmp '= (list 1 1 1))
                true
                "all numbers are equal")
  (check-equal? (eval-cmp '> (list 1 false 1)) 
                "err: CmpExpr expects evaluated args to be numbers"
                "wrong type of args"))

;; Strategy: Function composition

(define (eval-cmp op args)
  (if (andmap number? args)
        (calculate-cmp op args)
        "err: CmpExpr expects evaluated args to be numbers"))

;;------------------------------------------------------------------------------

;; get-if-branch : Result Expr Expr -> Expr
;; the if condition is evaluated if test is a boolean, else returns an ErrString

;; Examples

(begin-for-test
  (check-equal? (get-if-branch (make-arith '+ (list 2 4)) 2 4)
                "err: test in If-Exp should evaluate to a boolean"
                "test doesn't evaluate to a boolean")
  (check-equal? (get-if-branch true 2 4) 
                2
                "returns b1"))

;; Strategy: Function composition

(define (get-if-branch test b1 b2)
  (if (boolean? test)
      (if test b1 b2)
      "err: test in If-Exp should evaluate to a boolean"))

;;------------------------------------------------------------------------------

;; try-subst-call : Result ListOf<Result> -> Expr
;; if fn is lambda and the number of params is equal to the number of args, the
;; body of lambda is returned after substituting params of fn with corresponding
;; elements of args, else an ErrString is returned

;; Examples

(begin-for-test
  (check-equal? (try-subst-call (make-lam '(x) (make-arith '+ (list 2 'x))) 
                                (list 2))
                (make-arith '+ (list 2 2))
                "body returned after substitution")
  (check-equal? (try-subst-call (make-arith '+ (list 2 3)) (list 2)) 
                "err: function call sub expression didn't evaluate to lambda"
                "fn is not lambda"))

;; Strategy: Function composition

(define (try-subst-call fn args)
  (if (lambda? fn)
      (check-len-and-subst-call fn args)
      "err: function call sub expression didn't evaluate to lambda"))

;;------------------------------------------------------------------------------

;; check-len-and-subst-call : Lambda ListOf<Result> -> Expr
;; if the number of params of lamb is equal to the number of args, the body of 
;; lambda is returned after substituting params of lamb with corresponding 
;; elements of args, else an ErrString is returned

;; Examples

(begin-for-test
  (check-equal? (check-len-and-subst-call 
                 (make-lam '(x y) (make-arith '+ (list 2 'x 'y))) 
                 (list 2))
                "err: number of args don't match the number of params"
                "number of params is not equal to the number of args")
  (check-equal? (check-len-and-subst-call 
                 (make-lam '(x) (make-arith '+ (list 2 'x)))
                 (list 2)) 
                (make-arith '+ (list 2 2))
                "body returned after substitution"))

;; Strategy: Data decomposition on lamb: Lambda

(define (check-len-and-subst-call lamb args)
  (if (= (length (lam-params lamb)) (length args))
      (subst-call (lam-body lamb) (lam-params lamb) args)
      "err: number of args don't match the number of params"))

;;------------------------------------------------------------------------------

;; subst-call : Expr UniqueListOf<Param> ListOf<Result> -> Expr
;; substitutes elements of params0 present in body with corresponding elements 
;; of args0
;; Where: (length params0) is equal to (length args0)

;; Examples

(begin-for-test
  (check-equal? (subst-call (make-bool '> (list 2 'x)) '(x) (list 1)) 
                (make-bool '> (list 2 '1))
                "body returned after substitution"))

;; Strategy: Function composition

(define (subst-call body params0 args0)
  (local (; subst-call/a : UniqueListOf<Param> ListOf<Result> Expr -> Expr
          ; substitutes elements of params present in body with corresponding
          ; elements of args
          ; Where: (length params) = (length params). a is body with 
          ; substitutions made for Param from param0 not present in params
          ; Strategy: Data decomposition on params0 and args0: 
          ; UniqueListOf<Param> and ListOf<Result>
          (define (subst-call/a params args a)
            (cond
              [(empty? args) a]
              [else (subst-call/a 
                     (rest params)
                     (rest args)
                     (subst (first args) (first params) a))])))
    ;--IN--
    (subst-call/a params0 args0 body)))

;;------------------------------------------------------------------------------

;; get-lam-from-def : Def -> Lambda
;; returns a Lambda from the given def d

;; Examples

(begin-for-test
  (check-equal? (get-lam-from-def (make-def 'fn '(x) 'x)) 
                (make-lam '(x) 'x)
                "converts to lambda"))

;; Strategy: Data decomposition on d: Def

(define (get-lam-from-def d)
  (make-lam (def-params d) (def-body d)))

;;------------------------------------------------------------------------------

;; get-def : Var ListOf<Def> -> MayBe<Def>
;; either returns the Def with the name var or returns false if there is no Def
;; with the name var in defs

;; Examples

(begin-for-test
  (check-equal? (get-def 'fn (list (make-def 'fn '(x) 'x))) 
                (make-def 'fn '(x) 'x)
                "returns def iwth name 'fn")
  (check-equal? (get-def 'fn (list (make-def 'func '(x) 'x))) 
                false
                "no Def with the name 'fn"))

;; Strategy: Data decomposition on defs: ListOf<Def> 

(define (get-def var defs)
  (cond
    [(empty? defs) false]
    [else (if (is-def-name? var (first defs))
              (first defs)
              (get-def var (rest defs)))]))

;;------------------------------------------------------------------------------

;; is-def-name? : Var Def -> Boolean
;; checks if the name of the Def d is var

;; Examples

(begin-for-test
  (check-equal? (is-def-name? 'fn (make-def 'fn '(x) 'x)) 
                true
                "name of the Def is 'fn")
  (check-equal? (is-def-name? 'fn (make-def 'fun '(x) 'x)) 
                false
                "name of the Def is not 'fn"))

;; Strategy: Data decomposition on d: Def

(define (is-def-name? var d)
  (symbol=? var (def-name d)))

;;------------------------------------------------------------------------------

;; subst : Result Var Expr -> Expr
;; Replaces references to x in e with r.
;; Does not replace x with r if x occurs in the body of a lambda
;; that shadows x.

(begin-for-test
  (check-equal? (subst 4 'x 'x) 4 "x matches")
  (check-equal? (subst 4 'y 'x) 'x "y doesnt match")
  (check-equal? (subst 4 'y "error") "error" "nothing to substitute")
  (check-equal?
   (subst 4 'x (make-arith '+ '(x 5)))
   (make-arith '+ '(4 5))
   "subst in arith")
  (check-equal?
   (subst 4 'x (make-lam '(y) (make-arith '+ '(x y))))
   (make-lam '(y) (make-arith '+ '(4 y)))
   "subst in lambda")
  (check-equal?
   (subst 4 'x (make-lam '(x) (make-arith '+ '(x 5))))
   (make-lam '(x) (make-arith '+ '(x 5)))
   "does not subst shadowed vars in lambdas"))

;; Strategy: Data decomposition on e: Expr

(define (subst r x e)
  (cond
    [(number? e) e]
    [(boolean? e) e]
    [(var? e) (if (symbol=? x e)
                  r
                  e)]
    [(errstr? e) e]
    [(lambda? e) (if (member? x (lam-params e))
                     e
                     (make-lam (lam-params e) (subst r x (lam-body e))))]
    [(arith? e) (make-arith (arith-op e) 
                            (map (λ (ex) (subst r x ex)) (arith-args e)))]
    [(bool? e) (make-bool (bool-op e) 
                          (map (λ (ex) (subst r x ex)) (bool-args e)))]
    [(cmp? e) (make-cmp (cmp-op e) 
                        (map (λ (ex) (subst r x ex)) (cmp-args e)))]
    [(if-exp? e) (make-if-exp (subst r x (if-exp-test e))
                              (subst r x (if-exp-branch1 e))
                              (subst r x (if-exp-branch2 e)))]
    [(call? e) (make-call (subst r x (call-fn e))
                          (map (λ (ex) (subst r x ex)) (call-args e)))]))

;;------------------------------------------------------------------------------

;; get-index : Var UniqueListOf<Param> -> Natural
;; returns the index of item in ls0
;; Where: item is present in ls0

;; Examples

(begin-for-test
  (check-equal? (get-index 'x (list 'y 'x)) 
                1
                "'x is found at index 1"))

;; Strategy: Function composition

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

;;------------------------------------------------------------------------------

;; get-first-depth : ListOf<UniqueListOf<Param>> Var -> Natural
;; returns the ''index of the first UniqueListOf<Param>'' in which var is found 
;; in plist0
;; Where: var is present in plist0

;; Examples

(begin-for-test
  (check-equal? (get-first-depth (list (list 'y 'x) (list 'a 'b)) 'x) 
                0
                "the depth of 'x is 0"))

;; Strategy: Function composition

(define (get-first-depth plist0 var)
  (local (; get-first-depth/a: ListOf<UniqueListOf<Param>> Natural Boolean
          ; returns the index of the first UniqueListOf<Param> in which var is 
          ; found in plist
          ; Where: a is the number of lists in plist0 that are not in plist and
          ; not-found? keeps track of whether var was found or not
          ; Strategy: Data decomposition on plist: ListOf<UniqueListOf<Param>>
          (define (get-first-depth/a plist a not-found?)
            (cond
              [(empty? plist) a]
              [else (if (and (not (member? var (first plist))) not-found?)
                        (get-first-depth/a (rest plist) (add1 a) not-found?)
                        (get-first-depth/a (rest plist) a false))])))
    ;--IN--
    (get-first-depth/a plist0 ZERO true)))

;;------------------------------------------------------------------------------

;; get-first-index : ListOf<UniqueListOf<Param>> Var -> Natural
;; returns the ''index of var'' in the first UniqueListOf<Param> it is found in
;; plist0
;; Where: var is present in plist0

;; Examples

(begin-for-test
  (check-equal? (get-first-index (list (list 'y 'x) (list 'a 'b)) 'x) 
                1
                "the index of 'x is 0"))

;; Strategy: Function composition

(define (get-first-index plist0 var)
  (local (; get-first-index/a: ListOf<UniqueListOf<Param>> Natural Boolean
          ; returns the ''index of var'' in the first UniqueListOf<Param> it is
          ; found in plist
          ; Where: a is the index of var in the first list it is found in and
          ; found? keeps track of whether var index was found or not
          ; Strategy: Data decomposition on plist: ListOf<UniqueListOf<Param>>
          (define (get-first-index/a plist a found?)
            (cond
              [(empty? plist) a]
              [else (if (and (member? var (first plist)) (not found?))
                        (get-first-index/a (rest plist) 
                                     (get-index var (first plist))
                                     true)
                        (get-first-index/a (rest plist) a found?))])))
    ;--IN--
    (get-first-index/a  plist0 ZERO false)))
    
;;------------------------------------------------------------------------------

;; get-static-d : Var ListOf<UniqueListOf<Param>> -> StaticDist
;; returns the StaticDist of var given the list of params of all the lamdas
;; found till var was encountered.
;; Where: plist0 is in the reverse order of occurrence

;; Examples

(begin-for-test
  (check-equal? (get-static-d 'x (list (list 'a 'b)(list 'y 'x)))
                (list 1 1)
                "returns static distance of 'x")
  (check-equal? (get-static-d 'x (list (list 'a 'b)(list 'x 'y)(list 'y 'x)))
                (list 1 0)
                "considers the first 'x encountered because the list is in the
                 reverse order of occurrence"))

;; Strategy: Function composition

(define (get-static-d var plist0)
  (list (get-first-depth plist0 var) (get-first-index plist0 var)))

;;------------------------------------------------------------------------------

;; expr->expr/no-var : Expr -> ExprNoVar
;; Replaces Var in ex0 with StaticDist.
;; WHERE: there are no unbound variables in ex0.

;; Examples

(begin-for-test
  (check-equal?
   (expr->expr/no-var (make-lam '(x) 'x))
   (make-lam/no-var '(0 0))
   "basic lambda")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) (make-call 'x '(y z)))))
   (make-lam/no-var (make-lam/no-var (make-call '(1 0) '((1 1) (0 0)))))
   "nested lambdas")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) (make-call 'x '(y z)))))
   (make-lam/no-var (make-lam/no-var (make-call '(1 0) '((1 1) (0 0)))))
   "nested lambdas"))

;; Strategy: Function composition

(define (expr->expr/no-var ex0)
  (local 
    (; ex->ex/nv/a : Expr ListOf<UniqueListOf<Param>> -> ExprNoVar
     ; Replaces Var in ex with StaticDist.
     ; Where: plist is the list of all list of params encountered so far
     ; Strategy: Data decomposition on ex: Expr
     (define (ex->ex/nv/a ex plist)
       (cond
         [(number? ex) ex]
         [(boolean? ex) ex]
         [(var? ex) (get-static-d ex plist)]
         [(errstr? ex) ex]
         [(lambda? ex) (make-lam/no-var 
                        (ex->ex/nv/a (lam-body ex)
                                     (append (list (lam-params ex)) plist)))]
         [(arith? ex) (make-arith (arith-op ex)
                                  (map (λ (e) (ex->ex/nv/a e plist))
                                       (arith-args ex)))]
         [(bool? ex) (make-bool (bool-op ex) 
                                (map (λ (e) (ex->ex/nv/a e plist))
                                     (bool-args ex)))]
         [(cmp? ex) (make-cmp (cmp-op ex)
                              (map (λ (e) (ex->ex/nv/a e plist)) 
                                   (cmp-args ex)))]
         [(if-exp? ex) (make-if-exp (ex->ex/nv/a (if-exp-test ex) plist)
                                    (ex->ex/nv/a (if-exp-branch1 ex) plist)
                                    (ex->ex/nv/a (if-exp-branch2 ex) plist))]
         [(call? ex) (make-call (ex->ex/nv/a (call-fn ex) plist)
                                (map (λ (e) (ex->ex/nv/a e plist))
                                     (call-args ex)))])))
    
    
    ;--IN--
    (ex->ex/nv/a ex0 MT-LIST)))

;;------------------------------------------------------------------------------

;; expr=? : Expr Expr -> Boolean
;; Returns true if e1 and e2 are structurally equivalent, up to some
;; renaming of variable names.

;; Examples

(begin-for-test
  (check-true
   (expr=?
    (make-lam '(x) 'x)
    (make-lam '(y) 'y))
   "equivalent basic lambdas")
  (check-false
   (expr=?
    (make-lam '(x y) (make-call 'x '(y)))
    (make-lam '(y x) (make-call 'x '(y))))
   "not equivalent")
  (check-true
   (expr=?
    (make-lam '(y) (make-lam '(x) (make-call 'y '(x))))
    (make-lam '(x) (make-lam '(y) (make-call 'x '(y)))))
   "equivalent nested-lambdas"))

;; Strategy: Function composition

(define (expr=? e1 e2) 
  (equal? (expr->expr/no-var e1) (expr->expr/no-var e2)))



;; Tests
;;------------------------------------------------------------------------------

;; Tests for eval

(begin-for-test
  (check-equal? (eval '()) '())
  (check-equal? (first (eval (list (make-arith '* (list 1 2 3)))))
                6
                "multiplication")
  (check-pred errstr? (first (eval (list (make-arith '* (list 1 'hello 3)))))
              "error")
  (check-equal? (first (eval (list (make-bool 'and (list true true true)))))
                true
                "true")
  (check-equal? (first (eval (list (make-bool 'or (list false true true)))))
                true
                "true")
  (check-equal? (first (eval (list (make-cmp '= (list 1 1 1)))))
                true
                "equal")
  (check-equal? (first (eval (list (make-cmp '> (list 1 2 3)))))
                false
                "not greater than")
  (check-equal? (first (eval (list (make-cmp '> (list 3 2 1)))))
                true
                "greater than")
  (check-equal? (first (eval (list (make-cmp '< (list 1 2 2)))))
                false
                "has equal")
  (check-equal? (first (eval (list (make-cmp '> (list 1 2 2)))))
                false
                "has equal")
  (check-equal? (first (eval (list (make-cmp '< (list 1 2 0)))))
                false
                "not lesser than")
  (check-equal? (first (eval (list (make-if-exp (make-cmp '< (list 1 2 0))
                                                (make-arith '* (list 1 2 3))
                                                (make-arith '* (list 1 2 1))))))
                2
                "second branch evaluated")
  (check-equal? (first (eval (list (make-if-exp (make-cmp '< (list 1 2 7))
                                                (make-arith '/ (list 1 'h 3))
                                                (make-arith '* (list 1 2 1))))))
                "err: ArithExpr expects evaluated args to be numbers"
                "first branch evaluated")
  (check-equal? (first 
                 (eval 
                  (list 
                   (make-call (make-lam (list 'x 'y 'z)
                                        (make-arith '* (list 'x 'y 'z)))
                              (list (make-arith '* (list 1 2 1))
                                    2
                                    4)))))
                16
                "second branch evaluated")
  (check-pred errstr?
              (first 
               (eval 
                (list 
                 (make-call (make-lam (list 'x 'y 'z)
                                      (make-arith '* (list 'x 'y 'z)))
                            (list (make-arith '* (list 1 2 1))
                                  true
                                  4)))))
              "wrong kind of args")
  
  (check-equal? (eval (list 23 true "error")) 
                (list 23 true "error")
                "error is returned as is")
  (check-pred lambda? (first (eval (list 'f DEF))) "result is lambda")
  (check-pred errstr? (first (eval (list 'x))) "err: undefined var")
  (check-equal? (eval (list MK-ADD-DEF ADD5-DEF ADD5-OR-6-DEF
                            (make-call 'add5 '(10)) 
                            (make-call 'add5-or-6 '(200))
                            (make-call 'add5-or-6 '(-100))))
                (list 15 205 -94)
                "call-fn evaluates to a function"))

;;------------------------------------------------------------------------------

;; Tests for expr->expr/no-var

(begin-for-test
  (check-equal? (expr->expr/no-var 2) 
                2
                "number is returned as is")
  (check-equal? (expr->expr/no-var true) 
                true
                "boolean is returned as is")
  (check-equal? (expr->expr/no-var "error")
                "error"
                "ErrString is returned as is")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) (make-arith '+ '(y z)))))
   (make-lam/no-var (make-lam/no-var (make-arith '+ 
                                                 (list (list 1 1) (list 0 0)))))
   "nested lambdas")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) (make-cmp '> '(x z)))))
   (make-lam/no-var (make-lam/no-var (make-cmp '> 
                                                 (list (list 1 0) (list 0 0)))))
   "nested lambdas")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) (make-bool 'and '(z z)))))
   (make-lam/no-var (make-lam/no-var (make-bool 'and 
                                                 (list (list 0 0) (list 0 0)))))
   "nested lambdas")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) (make-bool 'and '(z z)))))
   (make-lam/no-var (make-lam/no-var (make-bool 'and 
                                                 (list (list 0 0) (list 0 0)))))
   "nested lambdas")
  (check-equal?
   (expr->expr/no-var (make-lam '(x y) (make-lam '(z) 
                                                 (make-if-exp 'y 'x 'z))))
   (make-lam/no-var (make-lam/no-var (make-if-exp (list 1 1) 
                                                  (list 1 0) 
                                                  (list 0 0))))
   "nested lambdas"))

;;------------------------------------------------------------------------------





