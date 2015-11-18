;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; coding: utf-8 -*-
; Ways to write DUDE in Lisp, and other clever one-liners.

(uiop:define-package :fare-puzzles/d00d
  (:use :common-lisp))
(in-package :fare-puzzles/d00d)

(format t "~X~%" 53261)
(format nil "~X~%" 53261)
(format t "~31r~%" 416530)
(let ((*print-base* 16)) (prin1 53261))
(not (mapcar 'princ (mapcar 'code-char '(68 85 68 69))))
(labels((f(x)(princ(code-char(+ 68(logand x 31))))(or(= x 1)(f(ash x -5)))))(f 33312))
(coerce(loop for i = 33312 then(ash i -5)while(> i 0)collect(code-char(+ 68(logand i 31))))'string)
(format t "~{D~S~}~%" '(u e))

(write 416530 :base 31) ; #31rDUDE
(not (mapcar 'princ (coerce "DUDE" 'list)))

(format t "~@R~29R" 51 837)
(do*((l'(595756 7869138747 528144 299002339))(x(pop l)(pop l))(n 31(1- n)))((not x))(format t "~VR~@[ ~]" n x l))

; former .signature by Krystof (Christophe Rhodes)
(macrolet ((f (a b) `(unless (equal ,a "") (princ (aref ,a 0)) (setf ,a (subseq ,a 1)) (,b))) (z (&rest a) `(format nil "~@{~35r~^ ~}" ,@a))) (let ((w (z 693 29204 28104384)) (x (z 1984050605838 12977))) (labels ((y () (f w z)) (z () (f x y))) (y) (terpri))))
; earlier one
(format t "~@(~@{~31r~^ ~}~)." 595756 9556552524 643802 496307950)


(defun magic-string (&optional (y 1))
  (let*(r
	(_(do-external-symbols(x :cl)(push x r)))
	(s(sort r #'string< :key #'string)))
    (ecase y
      (0
       (format nil "(~{~A~#[~:; ~]~})~%" s))
      (1
       (format nil "~{~A~^ ~}~%" s)))))

(defun cl-encode (x &optional (y 1))
  (let* ((s (magic-string y))
         (ll (length x))
         (ils
          (labels ((fnl (n l &optional i)
                     (search x s :start1 n :end1 (+ n l) :start2 i))
                   (fn (n)
                     (loop with i = 0
                           with rl = nil
                           for l from 1 to (- ll n)
                           for s = (fnl n l i)
                           while s
                           do (setf rl l i s)
                           finally (return (cons i rl)))))
            (loop with n = 0
                  for (i . l) = (fn n)
            while (< n ll)
            when l
            collect (progn (incf n l) (cons i l))
            else do (error "character ~S not found" (char x n))
            end))))
    (list (mapcar #'car ils) (mapcar #'cdr ils))))

(defun cl-decode (is ls &optional (y 1))
  (let ((s (magic-string y)))
    (map()(lambda(n l)(princ(subseq s n (+ n l))))is ls)))

(defun cl-encode-36 (ps ls &optional m)
  (loop with pe = (1+ (loop for p in ps maximize p))
	with le = (1+ (loop for l in ls maximize l))
	with m = (or m (* pe le))
	for p in ps
	for l in ls
	for b = 1 then (* b m)
	sum (* b (+ l (* p le))) into c
	finally (return (list c le m))))

(map()(lambda(n l)(princ(subseq #.(format nil "(~{~A~#[~:; ~]~})~%"(let(r)(do-external-symbols(x :cl)(push x r))(sort r #'string< :key #'string))) n (+ n l))))'(875 949 8 6010 4863 371 11)'(4 3 5 5 3 2 2))

(map()(lambda(n l)(princ(subseq(reduce(lambda(x y)(concatenate 'string x #(#\ )y))(let(r)(do-external-symbols(x :cl)(push (string x) r))(sort r #'string<)))n(+ n l)))(finish-output))'(874 948 7 6009 4862 370 10)'(4 3 5 5 3 2 2))

(map()(lambda(< >)(princ(subseq(reduce(lambda(< >)(concatenate'string <'(#\ )>))(let(>)(do-external-symbols(< :cl)(push(string <)>))(sort >'string<)))<(+ < >)))(finish-output))'(874 948 7 6009 4862 370 10)'(4 3 5 5 3 2 2))

(labels((^(>)(do-external-symbols(< :cl)(push(string <)>))(sort >'string<))(!(^ +)(princ(subseq(reduce #'?(^()))^(+ ^ +))(finish-output)))(?(< >)(concatenate'string <'(#\ )>)))(map()#'!'(874 948 7 6009 4862 370 10)'(4 3 5 5 3 2 2)))

clisp -norc -q -x "(quit(map()(lambda(n l)(princ(subseq(reduce(lambda(x y)(concatenate'base-string x #(#\\ )y))(let(r)(do-external-symbols(x :cl)(push (string x) r))(sort r #'string<))) n (+ n l))))'(874 948 7 6009 4862 370 10)'(4 3 5 5 3 2 2)))"

sbcl --noinform --eval "(progn (map()(lambda(n l)(princ(subseq(reduce(lambda(x y)(concatenate'base-string x #(#\\ )y))(let(r)(do-external-symbols(x :cl)(push (string x) r))(sort r #'string<))) n (+ n l))(finish-output)))'(874 948 7 6009 4862 370 10)'(4 3 5 5 3 2 2))(quit))"


;;; Ideas: encode symbols by number...

(labels((^(>)(do-external-symbols(< :cl)(push(string <)>))(sort >'string<))(!(^ +)(princ(subseq(reduce #'?(^()))^(+ ^ +))(finish-output)))(?(< >)(concatenate'string <'(#\ )>)))(map()#'!'(874 948 7 6009 4862 370 10)'(4 3 5 5 3 2 2)))

(map()(lambda(n l)(princ(subseq #.(format nil "(~{~A~#[~:; ~]~})~%"(let(r)(do-external-symbols(x :cl)(push x r))(sort r #'string< :key #'string))) n (+ n l))))'(875 949 8 6010 4863 371 11)'(4 3 5 5 3 2 2))

(labels(({(] &rest [)(apply([ ])[))([(])(elt(]())]))(](<)(do-external-symbols
(]`:cl)(push ] <))(sort <`string<`:key`string))(}(} {)({`816`2/9)({`688({`875
({`398()"~{~A ~}"(]()))}(+(*){(*)}))({`381))))({`561()#'}`(874,948,@`(7,6009)
4862 370,10)`(2,1 3,3 1,@`(0 0)))({`902)({`381))

(labels(({(] &rest [)(apply([ ])[))([(>)(elt(]())>))(](<)(do-external-symbols(] :cl)(push ] <))(sort <`string<`:key`string))(}({ + ^)({`816`1/5)({`688({`875({`398()"~{~A~^ ~}"(]())){(+ { +)))({`381)^))(do*(({`5248({`584 }`36063))([`874({`395 {`6))(]`4({`584 {`6))(}`#36RH4G6HUTA1NVC1ZHC({`395 }`36063)))((} [ ] ({`977 ]))({`902)({`381))))

; echo | sbcl --noinform --eval '(labels(({(] &rest [)(apply([ ])[))([(>)(elt(]())>))(](<)(do-external-symbols(] :cl)(push ] <))(sort <`string<`:key`string))(}({ + ^)({`816`1/5)({`688({`875({`398()"~{~A~^ ~}"(]())){(+ { +)))({`381)^))(do*(({`5248({`584 }`36063))([`874({`395 {`6))(]`4({`584 {`6))(}`#36RH4G6HUTA1NVC1ZHC({`395 }`36063)))((} [ ] ({`977 ]))({`902)({`381))))'

; clisp -norc -q -x '(quit(labels(({(] &rest [)(apply([ ])[))([(>)(elt(]())>))(](<)(do-external-symbols(] :cl)(push ] <))(sort <`string<`:key`string))(}({ + ^)({`816`1/5)({`688({`875({`398()"~{~A~^ ~}"(]())){(+ { +)))({`381)^))(do*(({`5248({`584 }`36063))([`874({`395 {`6))(]`4({`584 {`6))(}`#36RH4G6HUTA1NVC1ZHC({`395 }`36063)))((} [ ] ({`977 ]))({`902)({`381)))))'

; Decoded there: http://paste.lisp.org/display/88765

(defun make-sig-36-a (s &optional m (b 36) n)
  (destructuring-bind (ps ls) (cl-encode s 1)
    (destructuring-bind (c le m) (cl-encode-36 (cdr ps) (cdr ls) m)
      (list
       m
       (format nil "(labels(({(] &rest [)(apply([ ])[))([(>)(elt(]())>))(](<)(do-external-symbols(] :cl)(push ] <))(sort <`string<`:key`string))(}({ + ^)({`816`1/5)({`688({`875({`398()\"~~{~~A~~^ ~~}\"(]())){(+ { +)))({`381)^))(do*(({`~D({`584 }`~D))([`~D({`395 {`~D))(]`~D({`584 {`~D))(}`#~DR~vR({`395 }`~D)))((} [ ] ({`977 ]))({`902)({`381))))"
	       (or n 42) m (car ps) le (car ls) le b b c m)))))

(defun make-sig-36-b (s &optional m (b 36))
  (destructuring-bind (m e) (make-sig-36-a s)
    (format t "~A~%~A~%" m e)
    (eval (read-from-string e))))

(defvar *syms*
 (let*(>(_(do-external-symbols(< :cl)(push(string <)>)))(<(sort >'string<))
 )(coerce <'vector)))
(defvar *string*
  (format nil "~{~A~^ ~}" (coerce *syms* 'list)))
(defun symnum (x) (position(string x)*syms* :test'equal))
(defun numsym (n) (aref *syms* n))
(defun symnums (r) (dolist (i r)(format t "~&~A ~A~%" i (symnum i))))
(defun numsyms (r) (dolist (i r)(format t "~&~A ~A~%" i (numsym i))))

(defun rsymnum (r)
  (etypecase r
    (null nil)
    (symbol (or (symnum r) r))
    ;;(cons (cons (rsymnum (car r) (cdr r)))
    (string r)
    (list (mapcar #'rsymnum r))))

;; a better predicate to sort symbols, but too long:
(lambda (x y)(or (< (length x)(length y)) (string< x y)))

(with-output-to-string(&aux)
(dolist(&key *syms*)
(princ &key &aux))

(format &aux "~{~A~^ ~}" (coerce *syms* 'list)))

(let*
 (<
 (>(do-external-symbols(> :cl)(push > <)))
 (=(sort <'string< :key'string))
 (v(coerce ='vector))
)
 (length (reduce (lambda(x y)(concatenate 'string x" "y)) = :key 'string))
)


;(macrolet((m(&rest x)`(macrolet,@x))) (m(...)))

(loop for x being the external-symbols of :cl collect x)
(+(*)(*))

(do*((l'(595756 7869138747 528144 299002339))(x(pop l)(pop l))(n 31(1- n)))((not x))(format t "~VR~@[ ~]" n x l))
