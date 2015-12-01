;;; File: syntax.scm
;;;
;;; This file contains procedures that are taken from the Chapter 4
;;; interpreter.  They are used in two contexts:
;;;
;;; It is loaded by
;;;
;;;   eceval-support.scm to provide implementations of additional
;;;   machine-primitive operators in the register machines of Chapter
;;;   5.
;;;
;;;   compiler.scm to support syntax analysis in the compiler itself.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;      Representing expressions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Numbers, strings, and booleans are all represented as themselves.
;;; (Not characters though; they don't seem to work out as well
;;; because of an interaction with read and display.)

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (boolean? exp)
      ))

;;; variables -- represented as symbols

(define (variable? exp) (symbol? exp))

;;; quote -- represented as (quote <text-of-quotation>)

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;;; assignment -- represented as (set! <var> <value>)

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;; definitions -- represented as
;;;    (define <var> <value>)
;;;  or
;;;    (define (<var> <parameter_1> <parameter_2> ... <parameter_n>) <body>)
;;;
;;; The second form is immediately turned into the equivalent lambda
;;; expression.

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;;; lambda expressions -- represented as (lambda ...)
;;;
;;; That is, any list starting with lambda.  The list must have at
;;; least one other element, or an error will be generated.

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;; conditionals -- (if <predicate> <consequent> <alternative>?)

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;;;**following needed only to implement COND as derived expression,
;;; not needed by eceval machine in text.  But used by compiler

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


;;; or - defined in terms of if, as in r5rs

(define (or? exp) (tagged-list? exp 'or))
(define (or-exps exp) (cdr exp))
(define (or-first-exp exp) (car exp))
(define (or-rest-exp exp) (cdr exp))

;; argument is or-exps of a total or expression
(define (or->if exps)
  (if (null? exps)
      #f
      (let ((or-exp (or-first-exp exps)))
        (make-if or-exp or-exp (or->if (or-rest-exp exps))))))  

;;; sequences -- (begin <list of expressions>)

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;;; procedure applications -- any compound expression that is not one
;;; of the above expression types.

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;; let: syntactic sugar of an application of a lambda expression.
;;; ( let ((var1 val1) (var2 val2) ... ) (exp1) (exp2) ... ) turns into
;;; (lambda (var1 var2 ...) (exp1) (exp 2... )) val1 val2 ...

(define (let? exp) (tagged-list? exp 'let))
(define (let-operands exp) (cadr exp))
(define (let-variables oper) (car oper))
(define (let-values oper) (cadr oper))
(define (let-lambda exp) (cddr exp))

;; define a dummy function variable name and apply it.

(define (let->appl exp)
  (let ((operands (let-operands exp)))
    (make-begin
     (list
      `(define let-proc
         ,(make-lambda (map let-variables operands) (let-lambda exp)))
      (cons 'let-proc (map let-values operands))))))

;;; Derived expressions -- the only one we include initially is cond,
;;; which is a special form that is syntactically transformed into a
;;; nest of if expressions.

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-first-clause exp) (car exp))  ; added for eceval cond support
(define (cond-rest-clauses exp) (cdr exp))  ; renamed for clarity
(define (cond-no-clauses? exp) (null? exp)) ; added for eceval cond support
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cdr exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
;; end of Cond support

;; apply

(define (apply? exp) (tagged-list? exp 'apply))
(define (apply-proc exp) (cadr exp))
(define (apply-params exp) (caddr exp))

;; map

(define (map? exp) (tagged-list? exp 'map))
(define (map-proc exp) (cadr exp))
(define (map-list exp) (caddr exp))

