;; Problem Set 12 : eval.rkt

;; Language 
;;------------------------------------------------------------------------
#lang racket

(require "extras.rkt")
(require rackunit)
(require racket/list)

;; Provide Functions
;;------------------------------------------------------------------------

(provide mk-Program%)
(provide mk-Expr%)
(provide Program<%>)
(provide Expr<%>)
;(provide Result<%>)

;; Global Constants
;;------------------------------------------------------------------------

(define ZERO 0)
(define MT-LIST '())
(define TRUE true)
(define FALSE false)


;;------------------------------------------------------------------------


; A Program<%> represents an interface that is imlemneted by Program% class
; which contains the program object

(define Program<%>
  (interface()
    
    ; eval : -> ListOf<Result<%>>
    ; Evaluates expressions in the program to Result<%>s
    ; WHERE: A function may be called before it is defined.
    ; WHERE: The results have the same order as their original expressions 
    ; in the program.
    eval
    
    
    ; eval-with-defns : ListOf<Def> -> ListOf<Result<%>>
    ; Evaluates expressions in the program to Result<%>s with the list of all
    ; the defs in
    ; the program
    ; WHERE: A function may be called before it is defined.
    ; WHERE: The results have the same order as their original expressions in 
    ; the program.
    eval-with-defns
    
    ;get-prg
    
    ; gather-defns :  -> ListOf<Def>
    ; Extracts all the defs in the programs and maps them in a list
    ; to be used for evaluation of the program
    gather-defns
    
    ))


;;------------------------------------------------------------------------

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
    
    ; param-length-equal? : ListOf<Expr%> -> Maybe<ListOf<Result%>>
    ; Wrapper function that checks if the args provided
    ; are equal in length with the args of the Expr%
    param-length-equal?
    
    
    ; subst-call :  ListOf<Expr%> -> Maybe<ListOf<Result%>>
    ; Substitutes all the given argument values in the body
    ; of the given expressions
    subst-call
    
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
    ))

;;------------------------------------------------------------------------
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
    ; Strategy : Function Composition
    (define/public (eval defs)
      (eval-arith op (eval-expr-ls args defs)))
    
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    ; Strategy : Function Composition
    (define/public (subst r x)
      (new Arith% [op op] 
                 [args (map (λ (ex) (send ex subst r x)) args)]))
    
    
    ; eval-arith : ArithOp 2ListOf<Result%> -> Result%
    ; Performs the arithmetic operation op on args, if all elements of args are 
    ; numbers
    ; Strategy: Data decomposition on args: 2ListOf<Result%>
    (define (eval-arith op args)
      (local (; checking if all args are numbers
              (define is-valid (andmap (lambda (x)
                                         (number? (send x to-expr))) args)))
        ;--IN--
        (if is-valid
            (new Number% [ex (calculate-arith op (first args) (rest args))])
            (new ErrString% [ex "err: ArithExpr expects evaluated args to be numbers"]))))
    
    
    ; calculate-arith : ArithOp Number NEListOf<Number> -> Result
    ; performs the arithmetic operation op on the 2ListOf<Number> (cons first nums)
    ; Strategy : Data decomposition on op: ArithOp
    (define (calculate-arith op first nums)
      (cond
        [(plus? op) (foldl (λ (a b) (+ (send a to-expr) (send b to-expr))) first nums)]
        [(minus? op) (foldl (λ (a b) (- (send a to-expr) (send b to-expr))) first nums)]
        [(mult? op) (foldl (λ (a b) (* (send a to-expr) (send b to-expr))) first nums)]
        [(div? op) (if (member? ZERO nums)
                       (new ErrString% [ex "err: cannot divide by zero"])
                       (foldl (λ (a b) (/ (send b to-expr) (send a to-expr))) first nums))]))
    
    
    ; eval-expr-ls : ListOf<Expr%> ListOf<Def> -> ListOf<Result%>
    ; Evaluates each element of ls to a Result in the context of the list of Defs.
    ; Strategy: Function Composition
    (define (eval-expr-ls ls defs)
      (map (λ (ex) (send ex eval defs)) ls))
    
    
    ;; <Arithop predicates> : Arithop -> Boolean
    ;; Returns true if op is the Param indicated by the function name.
    ;; STRATEGY: Function composition
    (define (plus? op) (symbol=? '+ op))
    (define (minus? op) (symbol=? '- op))
    (define (mult? op) (symbol=? '* op))
    (define (div? op) (symbol=? '/ op))
    
    
    ; param-length-equal? : ListOf<Expr%> -> Maybe<ListOf<Result%>>
    ; Wrapper function that checks if the args provided
    ; are equal in length with the args of the arith object
    ; Strategy: Function Composition
    (define/public (param-length-equal? ars)
      #f)
    
    
    ; subst-call :  ListOf<Expr%> -> Maybe<ListOf<Result%>>
    ; Substitutes all the given argument values in the body
    ; of the given expressions
    ; Strategy: Function Composition
    (define/public (subst-call ars)
      #f)
    
    
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr which
    ; in this case is an arith
    ; Strategy : Function Composition
    (define/public (to-expr)
      (make-arith op args))
    
    
    ; to-expr/no-var/lst : ListOf<ListOf<UniqueParams>> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; by keeping  a track of List of parameters
    ; Strategy : Data decomposition on args : 2ListOf<Expr%>
    (define/public (to-expr/no-var/lst plist)
      (make-arith op
                  (map (λ (e) (send e to-expr/no-var/lst plist))
                       args)))
    
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; Strategy : Function Composition
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))

    ;--IN--
    ;(ex->ex/nv/a '())))
    
    (super-new)))

;;------------------------------------------------------------------------

; A Lam% is a (new Lam% [params UniqueListOf<Param>] [body Expr%]
; Represents the Lam class that implements the Expr<%> interface
; INTERP: 
; params : Contains the parameters inthe lambda function
; body : Contains the expression corresponding to the lambda function
(define Lam% 
  (class* object% (Expr<%>)
    (init-field params body)
    
    ; eval : ListOf<Def> -> Result<%>
    ; Evaluates the expression to a Result<%>,
    ; in the context of the given definitions.
    ; Strategy : Function Composition
    (define/public (eval defs)
      (new Lam% [params params] [body body]))
    
    
    ; subst : Result<%> Var -> Expr<%>
    ; Replaces references to the given var with the given Result<%>.
    ; Does not replace x with r if x occurs in the body of a lambda
    ; that shadows x.
    ; WHERE: the given Result<%> has no unbound variables.
    ; Strategy : Function Composition
    (define/public (subst r x)
      (if (member? x params)
          this
          (new Lam% [params params] [body (send body subst r x)])))
    
    
    
    ; param-length-equal? : ListOf<Expr%> -> Maybe<ListOf<Result%>>
    ; Wrapper function that checks if the args provided
    ; are equal in length with the params of the lam object
    ; Strategy: Function Composition
    (define/public (param-length-equal? args)
      (= (length params) (length args)))
    
    
    ; subst-call :  ListOf<Expr%> -> Maybe<ListOf<Result%>>
    ; Substitutes all the given argument values in the body
    ; of the given expressions
    ; Strategy: Function Composition
    (define/public (subst-call args0)
      (local (; subst-call/a : UniqueListOf<Param> ListOf<Result%> Expr% -> Expr%
              ; substitutes elements of params present in body with corresponding
              ; elements of args
              ; Where: (length params) = (length params). a is body with 
              ; substitutions made for Param from param0 not present in params
              ; Strategy: Data decomposition on params0 and args0: 
              ; UniqueListOf<Param> and ListOf<Result%>
              (define (subst-call/a paramsa argsa a)
                (cond
                  [(empty? argsa) a]
                  [else (subst-call/a 
                         (rest paramsa)
                         (rest argsa)
                         (send a subst (first argsa) (first paramsa)))])))
        ;--IN--
        (subst-call/a params args0 body)))
    
    
    ; to-expr : -> Expr
    ; Returns a representation of this expression as a non-object Expr which
    ; in this case is an arith
    ; Strategy : Function Composition
    (define/public (to-expr)
      (make-lam params body))
    
    
    ; to-expr/no-var/lst : ListOf<ListOf<UniqueParams>> -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; by keeping  a track of List of parameters
    ; Strategy : Data decomposition on args : 2ListOf<Expr%>
    (define/public (to-expr/no-var/lst plist)
      (make-lam/no-var 
       (send body to-expr/no-var/lst
             (append params plist))))
    
    
    ; to-expr/no-var : -> ExprNoVar
    ; Returns a representation of this expression as a non-object ExprNoVar.
    ; Strategy : Function Composition
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))

    ;--IN--
    ;(ex->ex/nv/a lst)))
    
    (super-new)))

;;------------------------------------------------------------------------
(define Number% 
  (class* object% (Expr<%>)
    (init-field ex)
    (define/public (eval defs)
      this)
    
    (define/public (subst r x)
      this)
    
    (define/public (param-length-equal? args)
      #f)
    
    (define/public (subst-call args)
      #f)
    
    (define/public (to-expr)
      ex)
    
    
    (define/public (to-expr/no-var/lst plist)
      ex)
    
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))
    
    (super-new)))
;;------------------------------------------------------------------------

(define Boolean% 
  (class* object% (Expr<%>)
    (init-field ex)
    (define/public (eval defs)
      ex)
    
    (define/public (subst r x)
      this)
    
    (define/public (param-length-equal? args)
      #f)
    
    (define/public (subst-call args)
      #f)
    
    (define/public (to-expr)
      ex)
    
    (define/public (to-expr/no-var/lst plist)
      ex)
    
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))
    
    (super-new)))

;;------------------------------------------------------------------------
(define ErrString% 
  (class* object% (Expr<%>)
    (init-field ex)
    (define/public (eval defs)
      ex)
    
    
    (define/public (subst r x)
      this)
    
    (define/public (param-length-equal? args)
      #f)
    
    (define/public (subst-call args)
      #f)
    
    (define/public (to-expr)
      ex)
    
    (define/public (to-expr/no-var/lst plist)
      ex)
    
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))
    
    (super-new)))

;;------------------------------------------------------------------------

(define Var% 
  (class* object% (Expr<%>)
    (init-field ex)
    
    (define/public (eval defs)
      (local (; gets the Def with the name var from the list of Defs
              (define df (get-def ex defs)))
        ;--IN--
        (if (equal? false df)
            (new ErrString% [ex "err: undefined var"])
            (eval-def df))))
    
    
    (define/public (subst r x)
      (if (symbol=? x ex)
                  r
                  this))
    
    (define (get-def var defs)
      (cond
        [(empty? defs) false]
        [else (if (is-def-name? var (first defs))
                  (first defs)
                  (get-def var (rest defs)))]))
    
    
    (define (eval-def d)
      (new Lam% [params (def-params d)] [body (def-body d)]))
    
    (define (is-def-name? var d)
      (symbol=? var (def-name d)))
    
    (define/public (param-length-equal? args)
      #f)
    
    (define/public (subst-call args)
      #f)
    
    (define/public (to-expr)
      ex)
    
    (define/public (to-expr/no-var/lst plist)
      (get-static-d ex plist))
    
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))

    ;--IN--
    ;(ex->ex/nv/a lst)))
    
    (define (get-static-d var plist0)
      (list (get-first-depth plist0 var) 
            (get-first-index plist0 var)))
    
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
                  [else (if (and (member? var (list (send (first plist) to-expr))) (not found?))
                            (get-first-index/a (rest plist) 
                                               (get-index var (list (send (first plist) to-expr)))
                                               true)
                        (get-first-index/a (rest plist) a found?))])))
        ;--IN--
        (get-first-index/a  plist0 ZERO false)))
    
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
                  [else (if (and (not (member? var (list (send (first plist) to-expr)))) not-found?)
                        (get-first-depth/a (rest plist) (add1 a) not-found?)
                        (get-first-depth/a (rest plist) a false))])))
    ;--IN--
        (get-first-depth/a plist0 ZERO true)))
    
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
    
    (super-new)))

;;------------------------------------------------------------------------
(define Bool% 
  (class* object% (Expr<%>)
  (init-field op args)
    
    
    (define/public (eval defs)
      (eval-bool op (eval-expr-ls args defs)))
    
    
    (define/public (subst r x)
      (new Bool% [op op] 
                [args (map (λ (ex) (send ex subst r x)) args)]))
    
    (define (eval-bool op args)
      (if (andmap boolean? args)
          (new Bool% [ex (calculate-bool op args)])
          (new ErrString% [ex "err: BoolExpr expects evaluated args to be booleans"])))
    
    (define (eval-expr-ls ls defs)
      (map (λ (ex) (send ex eval defs)) ls))
    
    
    (define (calculate-bool op args)
      (cond
        [(and? op) (foldr (λ (a b) (and (send a to-expr) (send b to-expr))) TRUE args)]
        [(or? op) (foldr (λ (a b) (or (send a to-expr) (send b to-expr))) FALSE args)]))
    
    (define (and? op) (symbol=? 'and op))
    (define (or? op) (symbol=? 'or op))
    
    (define/public (param-length-equal? args)
      #f)
    
    (define/public (subst-call args)
      #f)
    
    (define/public (to-expr)
      (make-bool op args))
    
    
    (define/public (to-expr/no-var/lst plist)
      (make-bool op 
                 (map (λ (e) (send e to-expr/no-var/lst plist))
                      args)))
    
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))

    ;--IN--
    ;(ex->ex/nv/a lst)))
    
    (super-new)))

;;------------------------------------------------------------------------
(define Cmp% 
  (class* object% (Expr<%>)
  (init-field op args)
    
    (define/public (eval defs)
      (eval-cmp op (eval-expr-ls args defs)))
    
    (define/public (subst r x)
      (new Cmp% [op op] 
                [args (map (λ (ex) (send ex subst r x)) args)]))
    
    (define (eval-expr-ls ls defs)
      (map (λ (ex) (send ex eval defs)) ls))
    
    
    (define (eval-cmp op args)
      (if (andmap number? args)
          (new Bool% [ex (calculate-cmp op (first args) (rest args))])
          (new ErrString% [ex "err: CmpExpr expects evaluated args to be numbers"])))
    
    
    (define (calculate-cmp op first args)
      (local (; full list of arguments
              (define all (cons (send first to-expr) (send args to-expr))))
        ;--IN--
        (cond
          [(comp-eq? op) (andmap (λ (a) (= (send a to-expr) (send first to-expr))) args)]
          [(comp<? op) (and (not (not-unique? args)) (equal? (sort all <) all))]
          [(comp>? op) (and (not (not-unique? args)) (equal? (sort all >) all))])))
    
    
    (define (not-unique? ls)
      (cond
        [(empty? ls) FALSE]
        [else (if (ormap (λ (n) (= (send n to-expr) (send (first ls) to-expr))) (rest ls))
                  TRUE
                  (not-unique? (rest ls)))]))
    
    
    (define (comp-eq? op) (symbol=? '= op))
    
    (define (comp<? op) (symbol=? '< op))
    
    (define (comp>? op) (symbol=? '> op))
    
    
    (define/public (param-length-equal? args)
      #f)
    
    (define/public (subst-call args)
      #f)
    
    
    (define/public (to-expr)
      (make-cmp op args))
    
    (define/public (to-expr/no-var/lst plist)
      (make-cmp op 
                 (map (λ (e) (send e to-expr/no-var/lst plist))
                      args)))
    
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))

    ;--IN--
    ;(ex->ex/nv/a lst)))
    
    (super-new)))
;;------------------------------------------------------------------------

(define If-Exp% 
  (class* object% (Expr<%>)
  (init-field test branch1 branch2)
    
    (define/public (eval defs)
      (send (get-if-branch (send test eval defs)
                           branch1
                           branch2)
            eval
            defs))
    
    (define/public (subst r x)
      (new If-Exp% [test (send test subst r x)]
                  [branch1 (send branch1 subst r x)]
                  [branch2 (send branch2 subst r x)]))
    
    (define (get-if-branch test b1 b2)
      (if (boolean? test)
          (if test b1 b2)
          (new ErrString% [ex "err: test in If-Exp should evaluate to a boolean"])))
    
    (define (eval-expr-ls ls defs)
      (map (λ (ex) (send ex eval defs)) ls))
    
    (define/public (param-length-equal? args)
      #f)
    
    (define/public (subst-call args)
      #f)
    
    (define/public (to-expr)
      (make-if-exp test branch1 branch2))
    
    (define/public (to-expr/no-var/lst plist)
      (make-if-exp (send test to-expr/no-var/lst plist)
                   (send branch1 to-expr/no-var/lst (send plist to-expr))
                   (send branch2 to-expr/no-var/lst (send plist to-expr))))
    
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))

    ;--IN--
    ;(ex->ex/nv/a lst)))
    
    (super-new)))
;;------------------------------------------------------------------------

(define Call% 
  (class* object% (Expr<%>)
  (init-field fn args)
    
    (define/public (eval defs)
      (send (try-subst-call (send fn eval defs) 
                            (eval-expr-ls args defs))
            eval
            defs))
    
    (define/public (subst r x)
      (new Call% [fn (send fn subst r x)] 
                [args (map (λ (ex) (send ex subst r x)) args)]))
    
    (define (try-subst-call fn args)
      ;(if (lambda? (send fn to-expr))
          (check-len-and-subst-call fn args))
      ;    (new ErrString% [ex "err: function call sub expression didn't evaluate to lambda"])))
    
    (define (eval-expr-ls ls defs)
      (map (λ (ex) (send ex eval defs)) ls))
    
    
    (define (check-len-and-subst-call lamb args)
      (if (send lamb param-length-equal? args)
          (send lamb subst-call args)
          (new ErrString% [ex "err: number of args don't match the number of params"])))
    
    (define/public (param-length-equal? args)
      #f)
    
    (define/public (subst-call args)
      #f)
    
    (define/public (to-expr)
      (make-call fn args))
    
    
    (define/public (to-expr/no-var/lst plist)
      (make-call (send fn to-expr/no-var/lst plist)
                 (map (λ (e) (send e to-expr/no-var/lst plist))
                      args)))
    
    (define/public (to-expr/no-var)
      (to-expr/no-var/lst '()))

    ;--IN--
    ;(ex->ex/nv/a lst)))
    
    (super-new)))

;;------------------------------------------------------------------------

(define Program% 
  (class* object% (Program<%>)
    (init-field prg)
    (define/public (eval)
      (eval-with-defns (gather-defns)))
    
    (define/public (eval-with-defns defs)
      (local (; all the Defs in the program
          ; eval-local : Program -> ListOf<Result>
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
    
    (define/public (get-prg)
      prg)
    
    
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

;;------------------------------------------------------------------------
(define (mk-Program% p)
  (cond
    [(empty? p) (new Program% [prg MT-LIST])]
    [(not (def? (first p))) (new Program% [prg (list (mk-Expr% (first p)) (mk-Program% (rest p)))])]
    [else (new Program% [prg (list (mk-def (first p)) (mk-Program% (rest p)))])]))

;;------------------------------------------------------------------------
(define (mk-def df)
  (make-def (def-name df) 
           (def-params df)
           (mk-Expr% (def-body df))))
;;------------------------------------------------------------------------
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

;;------------------------------------------------------------------------
(define (mk-Expr%-ls ls)
  (map (λ (ex) (mk-Expr% ex)) ls))
;;------------------------------------------------------------------------

(define (member? item list)
  (ormap (lambda (ls-item)
           (eq? item ls-item)) list))
;;------------------------------------------------------------------------

(define (var? e) (symbol? e))

;;------------------------------------------------------------------------
(define (errstr? e) (string? e))
;;------------------------------------------------------------------------
(define-struct def (name params body) #:transparent)
(define-struct arith (op args) #:transparent)
(define-struct bool (op args) #:transparent)
(define-struct cmp (op args) #:transparent)
(define-struct if-exp (test branch1 branch2) #:transparent)
(define-struct call (fn args) #:transparent)
(define-struct lam (params body) #:transparent)
(define-struct lam/no-var (body) #:transparent)
(define (lambda? e) (lam? e))

;;------------------------------------------------------------------------



(define PGM-MIXED (list (make-def 'testfn (list 'x 'y)
                    (make-arith '- (list (make-arith '* (list 'x 'y))
                                         (make-arith '+ (list 'x 'y)))))
          (make-call 'testfn (list 10 11))))

(define PGM-EXP (list (make-arith '/ (list 6 1 3))))

;(send (mk-Program% PGM-MIXED) eval)

;(send (mk-Expr% (make-lam '(x y) (make-lam '(z) 
;                           (make-call 'x '(y z))))) to-expr/no-var)
;(mk-Expr% (make-lam '(x y) (make-lam '(z) 
;                           (make-call 'x '(y z)))))

;(send (mk-Program% (list (make-def 'testfn (list 'x 'y)
;                    (make-arith '- (list (make-arith '* (list 'x 'y))
;                                         (make-arith '+ (list 'x 'y)))))
;          (make-call 'testfn (list 10 11)))) eval)



;--------------------------
;; expr=? : Expr% Expr% -> Boolean
;; Returns true if e1 and e2 are structurally equivalent, up to some
;; renaming of variable names.

;; Example
(begin-for-test 
   (check-true
   (expr=?
   (mk-Expr% (make-lam '(y) (make-lam '(x) (make-call 'y '(x)))))
   (mk-Expr% (make-lam '(x) (make-lam '(y) (make-call 'x '(y))))))
   "equivalent nested-lambdas")
   (check-false
   (expr=?
    (mk-Expr%(make-lam '(x y) (make-call 'x '(y))))
    (mk-Expr% (make-lam '(y x) (make-call 'x '(y)))))
   "not equivalent"))

;; Strategy: Function composition
(define (expr=? e1 e2) 
  (equal? 
   (send e1 to-expr/no-var ) 
   (send e2 to-expr/no-var )))

;--------------------
;; Tests
;;------------------------------------------------------------------------------

;; Tests for eval
(begin-for-test
  (check-equal?  (send (mk-Program% '()) eval)  '()))