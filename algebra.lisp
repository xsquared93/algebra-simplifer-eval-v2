(ql:quickload :prove)

(in-package :cl-user)
(defpackage algebra
  (:use :cl
	:prove)
  (:export :compute))
	 
  
(in-package :algebra)

;; to make calls to `compute` once this file is loaded on to the REPL.
(use-package :algebra)


;; an Exp is one of:
;;   - term; or
;;   - expression.

;; a Term is one of:
;;   - number; or
;;   - monomial.

;; an Expression is one of:
;;   - (eql (car exp) '+); or
;;   - (eql (car exp) '-); or
;;   - (eql (car exp) '*).

;; Exp -> Exp
;; adds, subtracts, and multiplies exps.
;; given: (compute '(2 x 3)); expect: '(2 x 3)
;; given: (compute '(+ (2 x 3) (2 x 3))); expect: '(4 x 3)
;; given: (compute '(- (5 x 3) (2 x 3))); expect: '(3 x 3)
(defun compute (exp)
  (cond ((term? exp) exp)
	((expression? exp)
	 (make (compute (first-expression exp))
	       (compute (second-expression exp))
	       (operator exp)))
	(t (error "Unknown Expression Type --COMPUTE"))))

;; Exp -> bool
;; determines if the given Exp is a number or monomial. If it is then Exp is a term;
;; otherwise it's not.
(defun term? (exp)
  (or (numberp exp)
      (monomial? exp)))

;; Exp -> bool
;; determines if an Exp is a monomial.
(defun monomial? (exp)
  (and (consp exp)
       (numberp (car exp))
       (symbolp (car (cdr exp)))
       (numberp (car (cdr (cdr exp))))))
       
;; Exp -> bool
;; determines if argument is an Expression. If (car exp) is either '+, '-, or '* then
;; it is an Expression; otherwise it is not.
(defun expression? (exp)
  (or (eql (car exp) '+)
      (eql (car exp) '*)
      (eql (car exp) '-)))

;; Expression -> Exp
;; returns the first operand given an expression.
(defun first-expression (expression) (car (cdr expression)))

;; Expression -> Exp
;; returns the second operand given an expression.
(defun second-expression (exp) (car (cdr (cdr exp))))

;; Expression -> symbol
;; returns the operator of a given expression.
(defun operator (exp) (car exp))

;; Exp Exp -> pair
;; `make` makes additions and subtractions and products.
;; (make 2 3 '+) -> '(+ 2 3)
;; (make 2 3 '-) -> '(- 2 3)
(defun make (e1 e2 op)
  (cond ((eql op '+)
	 (make-addition e1 e2))
        ((eql op '-)
	 (make-subtraction e1 e2))
	(t (make-product e1 e2))))
	 
;; Exp Exp -> procedure
(defun make-addition (e1 e2)
  (make-exp e1
	    e2
	    '+
	    #'add-monomials))

;; Exp Exp -> procedure
(defun make-subtraction (e1 e2)
  (make-exp e1
	    e2
	    '-
	    #'subtract-monomials))

;; Exp Exp symbol procedure -> pair
;; this is a higher order procedure that makes expressions.
(defun make-exp (e1 e2 op term)
  (cond ((eql e1 0) e2)
	((eql e2 0) e1)
	((&number? e1 e2) (+ e1 e2))
	((&monomial? e1 e2)
	 (funcall term e1 e2))
	(t (list op e1 e2))))

;; make-product

;; Exp Exp -> bool
;; determines if both arguments are numbers; if yes then it evaluates to true;
;; otherwise it evaluates to false.
(defun &number? (e1 e2)
  (and (numberp e1)
       (numberp e1)))

;; Exp Exp -> bool
;; if both arguments are monomials then it evalautes to true; otherwise false.
(defun &monomial? (e1 e2)
  (and (monomial? e1)
       (monomial? e2)))

;; Exp Exp -> procedure[compute-monomials]
(defun add-monomials (e1 e2)
  (compute-monomials e1
		     e2
		     #'+
		     '+))
;; Exp Exp -> compute-monomials
(defun subtract-monomials (e1 e2)
  (compute-monomials e1
		     e2
		     #'-
		     '-))

;; Exp Exp procedure symbol -> pair
(defun compute-monomials (e1 e2 term op)
  (if (same-variable? e1 e2)
      (make-monomial (funcall term (coeff e1) (coeff e2))
		     (variablex e1)
		     (exponent e1))
      (list op e1 e2)))

;; monomial -> symbol
;; given a monomial it returns the variable.
(defun variablex (exp) (car (cdr exp)))

;; monomial monomial -> bool
(defun same-variable? (e1 e2)
  (eql (variablex e1) (variablex e2)))

;; number symbol exponent -> pair
(defun make-monomial (coeff var exponent)
  (list coeff var exponent))

;; monomial -> number
;; returns the coefficient of a given monomial.
(defun coeff (exp) (car exp))

;; monomial -> number
;; returns the exponent of a given monomial.
(defun exponent (exp) (car (cdr (cdr exp))))

;;; tests

(plan 3)

(is (compute '(2 x 3))
    '(2 x 3))
(is (compute '(+ (2 x 3) (2 x 3)))
    '(4 x 3))

(is (compute '(- (5 x 3) (2 x 3)))
    '(3 x 3))

(finalize)
