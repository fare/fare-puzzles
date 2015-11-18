(uiop:define-package :fare-puzzles/glat1
  (:use :common-lisp))
(in-package :fare-puzzles/glat1)

; Solving GLAT question #1:
; WWWDOT - GOOGLE = DOTCOM

; Simplifying equation:
; WWWDOT - GOOGLE - DOTCOM = 0
; W*111000 + D*100 + O*10 + T - G*100100 - O*11000 - L*10 - E
; - D*100000 - O*10010 - T*1000 - C*100 - M = 0
; W*111000 - G*100100 - D*99900 - O*21000 - T*999 - C*100 - L*10 - M - E = 0

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro with-gensyms (syms &body body)
  "Replaces given symbols with gensyms. Useful for creating macros.
This version by Paul Graham in On Lisp."
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms) ,@body))
(defmacro pop-smallest (place &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (with-gensyms (list val rest)
    `(let* (,@(mapcar #'list vars vals)
	       ,@store-vars
	    (,list ,reader-form)
	    (,val (car ,list))
	    (,rest (cdr ,list)))
	  (multiple-value-setq ,store-vars ,rest)
	  ,writer-form
          ,val))))
(defmacro pop-biggest (place &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (with-gensyms (list val rest)
    `(let* (,@(mapcar #'list vars vals)
	       ,@store-vars
	    (,list ,reader-form)
	    (,val (car (last ,list)))
	    (,rest (remove ,val ,list)))
	  (multiple-value-setq ,store-vars ,rest)
	  ,writer-form
          ,val))))
)

(defstruct (equation (:conc-name $))
  W G D O TT C L M E)

(defun show-eq (equation)
   (with-slots (W G D O TT C L M E) equation
	(format t "~A~A~A~A~A~A - ~A~A~A~A~A~A = ~A~A~A~A~A~A~%"
	  W W W D O TT  G O O G L E  D O TT C O M)))

(defun solutionp-1 (equation)
   (with-slots (W G D O TT C L M E) equation
	(zerop (- (+ (* 111000 W) (* 100 D) (* 10 O) TT)
		  (+ (* 100100 G) (* 11000 O) (* 10 L) E)
		  (+ (* 100000 D) (* 10010 O) (* 1000 TT) (* 100 C) M)))))

(defun solutionp (equation)
   (with-slots (W G D O TT C L M E) equation
	(= (* 111000 W)
	   (+ (* 100100 G) (* 99900 D) (* 21000 O)
	      (* 999 TT) (* 100 C) (* 10 L) M E))))

; W*111000 - G*100100 - D*99900 - O*21000 - T*999 - C*100 - L*10 - M - E = 0
(defvar *variables* '(W G D O TT C L M E))
(defvar *coefficients* '(111000 -100100 -99900 -21000 -999 -100 -10 -1 -1))
(defvar *ranges* '((3 9) (1 9) (1 9) (0 9) (0 9) (0 9) (0 9) (0 9) (0 9)))

(defun iota (n m) (loop for i from n to m collect i))

(defstruct (problem (:conc-name $))
  (instantiated (make-equation))
  (remaining (make-remaining))
  (unalloted-digits (iota 0 9))
  (partial-sum 0))

(defstruct (searchvar (:conc-name $))
  name coefficient #| range |#)

(defun make-remaining ()
  (loop for v in *variables* for c in *coefficients*
#|	for r in *ranges* for min = (car r) for max = (cadr r) |#
	collect (make-searchvar :name v :coefficient c 
				#|:range (iota min max)|#)))

(defvar *problem* nil)

(defun problem-solved-p ()
  (and (null ($remaining *problem*))
       (solutionp ($instantiated *problem*))))

(defun output-solution ()
  (format t "FOUND A SOLUTION: ")
  (show-eq ($instantiated *problem*)))

(defun simplify-constraints ()
  #| Reduce the target set for all variables XXX |#
 nil)

(defun maximum-sum ()
  (+ ($partial-sum *problem*)
     (loop for var in ($remaining *problem*)
	   for val in ($unalloted-digits *problem*)
	   sum (* ($coefficient var) val))))
(defun minimum-sum ()
  (+ ($partial-sum *problem*)
     (loop for var in ($remaining *problem*)
	   for val in (reverse ($unalloted-digits *problem*))
	   sum (* ($coefficient var) val))))

(defun get-min-max-sums ()
  (loop for var in ($remaining *problem*)
	with minvals = ($unalloted-digits *problem*)
	with maxvals = ($unalloted-digits *problem*)
	with minsum = ($partial-sum *problem*)
	with maxsum = ($partial-sum *problem*)
	with minval with maxval
	for coeff = ($coefficient var)
	do (progn
	     (if (> coeff 0)
		 (setf minval (pop-smallest minvals)
		       maxval (pop-biggest maxvals))
		 (setf minval (pop-biggest minvals)
		       maxval (pop-smallest maxvals)))
	     (incf minsum (* coeff minval))
	     (incf maxsum (* coeff maxval)))
	finally (return (values minsum maxsum))))

(defun problem-obviously-impossible-p ()
  (multiple-value-bind (minsum maxsum) (get-min-max-sums)
    (not (<= minsum 0 maxsum))))
(defun propagate-constraint (problem variable value)
  (pop ($remaining problem))
#|  (setf ($remaining problem)
	(remove variable ($remaining problem))) |#
#|
	(loop for var in ($remaining problem)
	      unless (eq variable var)
	      collect (let ((remove value ($range var))
			    (make-searchvar :name ($name var)
					    :coefficient ($coefficient var)
					    :range (remove value ($range var))))))
|#
  (setf ($unalloted-digits problem)
	(remove value ($unalloted-digits problem)))
  (incf ($partial-sum problem)
	(* ($coefficient variable) value)))

(defun fail () (throw 'fail nil))

(defun variables-remain-p ()
  (consp ($remaining *problem*)))

(defun pick-most-constrained-variable ()
  (car ($remaining *problem*)))

(defmacro with-problem (problem &body body)
  `(catch 'fail (let ((*problem* ,problem)) ,@body)))

(defun search-for-solutions ()
   (simplify-constraints)
   (cond
     ((problem-solved-p) (output-solution))
     ((problem-obviously-impossible-p) (fail))
     (t (search-harder))))

(defmacro for-all-possible-values ((variable) &body body)
  (declare (ignore variable))
  `(dolist (value ($unalloted-digits *problem*)) ,@body))
;  `(dolist (value ($range ,variable)) ,@body))
;   `(call-for-all-possible-instantiations ,variable (lambda (value) ,@body))

(defun call-for-all-possible-values (variable thunk)
  (dolist (value ($range variable))
	(funcall thunk value)))

(defun instantiate-variable (problem variable value)
  (let ((problem (copy-structure problem)))
    (setf ($instantiated problem) (copy-structure ($instantiated problem))
	  (slot-value ($instantiated problem) ($name variable)) value)
    (propagate-constraint problem variable value)
    problem))

(defun search-harder ()
   (when (variables-remain-p)
      (let ((variable (pick-most-constrained-variable)))
	(for-all-possible-values (variable)
	   (with-problem (instantiate-variable *problem* variable value)
	     (search-for-solutions))))))

(defun do-it ()
  (with-problem (make-problem) (search-for-solutions)))

(defun make-simplified-problem ()
  (let* ((p (make-problem))
	 (i ($instantiated p)))
    (setf ($w i) 7 ($g i) 1 ($d i) 5 ($o i) 8 ($tt i) 9 ($c i) 4
	  ($remaining p) (nthcdr 6 ($remaining p))
	  ($partial-sum p) 9
	  ($unalloted-digits p) '(0 2 3 6))
    p))

;(defun do-it ()
; (with-problem (make-simplified-problem) (search-for-solutions)))


;; For glat 10: 
; Google "infinite lattice resistors"
; http://rec-puzzles.org/new/sol.pl/physics/resistors

;;; For all glats:
; mathpuzzle.com => http://mathworld.wolfram.com/news/2004-10-13/google/
