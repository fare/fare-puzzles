(uiop:define-package :fare-puzzles/misc/tak
  (:use :common-lisp))
(in-package :fare-puzzles/misc/tak)

;; At ILC'2012, Tak challenged us to write a palindromic program,
;; modulo parenthesis inversion,
;; and without using any reader macro ;; such as ' ` ;
;; His next slide had a solution, which additionally was a function definition,
;; but it had unused variable warnings in SBCL.
;; I could immediately come up with the following program,
;; which is trivial, no function definition, and produces warnings:
;; (let (let tel) tel)
;;
;; The natural next challenge was to produce
;; the shortest palindromic quine that doesn't issue warnings...

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun quotep (x) (and (consp x) (consp (cdr x)) (null (cddr x)) (eq 'quote (car x))))
  (set-pprint-dispatch '(satisfies quotep) (lambda (s x) (format s "(~W ~W)" 'quote (cadr x))))
  (setf *print-level* nil *print-length* nil *print-pretty* nil *print-case* :downcase))

(defun rev (x &aux (r (reverse x))) (loop for i below (length r) for c = (char r i) do (case c (#\( (setf (char r i) #\))) (#\) (setf (char r i) #\()))) r)
(defun revp (x) (equal x (rev x)))
(defun st (x) (with-output-to-string (s) (with-standard-io-syntax (write x :readably t :level nil :length nil :case :downcase :pretty nil :stream s) (terpri))))
(defun palp (x &aux (s (st x))) (unless (revp s) (error "Not a palindrome")) (format t "Length: ~D~%Program:~%~A~%" (length s) s) t)
(defun pqp (x &aux (s (st x))) (unless (revp s) (error "Not a palindrome")) (let* ((n (eval x)) (q (funcall n))) (unless (equal x q) (error "Not a quine")) (format t "Length: ~D~%Program:~%~A~%" (length s) s)) t)

(palp'
(defun pal (funcall &aux pal constantly defun quote etouq nufed yltnatsnoc lap (&aux etouq) (xua& yltnatsnoc) llacnuf) ((lambda (x) x (setf nufed funcall)) etouq) (quote ((llacnuf defun ftes) x (x) adbmal)) (funcall (constantly &aux) (quote xua&) pal constantly defun quote etouq nufed yltnatsnoc lap xua& llacnuf) lap nufed)
)

(pqp'
(defun pq (&aux funcall pq constantly defun quote etouq nufed yltnatsnoc qp (&aux etouq) (xua& yltnatsnoc) llacnuf) ((lambda (q) (setf q (quote (lambda (q) (labels ((rev (x) (etypecase x (cons (reverse (mapcar (function rev) x))) (symbol (intern (reverse (symbol-name x))))))) (let ((qq (copy-tree (quote (defun pq (&aux funcall pq constantly defun quote etouq nufed yltnatsnoc qp (&aux etouq) (xua& yltnatsnoc) llacnuf) ((lambda (q) (setf q (quote qq) nufed (eval (list q (list (quote quote) q))))) etouq) funcall))))) (rplaca (cdaddr (caddar (cadddr qq))) q) (append qq (rev qq)))))) nufed (eval (list q (list (quote quote) q))))) etouq) funcall llacnuf (quote (((((q (etouq etouq) tsil) q tsil) lave) defun ((((((qq ver) qq dneppa) (q (((qq rdddac) raddac) rddadc) acalpr) (((((llacnuf (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (qq etouq) q ftes) (q) adbmal)) (funcall (constantly &aux) (quote xua&) pq constantly defun quote etouq nufed yltnatsnoc qp llacnuf xua&) qp nufed) etouq) eert-ypoc) qq)) tel) (((((((x eman-lobmys) esrever) nretni) lobmys) (((x (ver noitcnuf) racpam) esrever) snoc) x esacepyte) (x) ver)) slebal) (q) adbmal) etouq) q ftes) (q) adbmal)) (funcall (constantly &aux) (quote xua&) pq constantly defun quote etouq nufed yltnatsnoc qp llacnuf xua&) qp nufed)
) ; 1295

(pqp'
(defun pq (&aux funcall pq defun nufed quote etouq (&aux etouq) (xua& etouq) qp llacnuf) ((lambda (q) (setf q (quote (lambda (q) (labels ((rev (x) (etypecase x (cons (reverse (mapcar (function rev) x))) (symbol (intern (reverse (symbol-name x))))))) (let ((qq (copy-tree (quote (defun pq (&aux funcall pq defun nufed quote etouq (&aux etouq) (xua& etouq) qp llacnuf) ((lambda (q) (setf q (quote qq) pq (constantly t) nufed (eval (list q (list (quote quote) q))))) etouq) funcall))))) (rplaca (cdaddr (caddar (cadddr qq))) q) (append qq (rev qq)))))) pq (constantly t) nufed (eval (list q (list (quote quote) q))))) etouq) funcall llacnuf (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (t yltnatsnoc) qp ((((((qq ver) qq dneppa) (q (((qq rdddac) raddac) rddadc) acalpr) (((((llacnuf (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (t yltnatsnoc) qp (qq etouq) q ftes) (q) adbmal)) (funcall pq (quote &aux) (quote xua&) quote etouq defun nufed qp llacnuf xua&) qp nufed) etouq) eert-ypoc) qq)) tel) (((((((x eman-lobmys) esrever) nretni) lobmys) (((x (ver noitcnuf) racpam) esrever) snoc) x esacepyte) (x) ver)) slebal) (q) adbmal) etouq) q ftes) (q) adbmal)) (funcall pq (quote &aux) (quote xua&) quote etouq defun nufed qp llacnuf xua&) qp nufed)
) ; 1259

(pqp'
(defun pq (&aux funcall pq defun nufed quote etouq (&aux etouq) (xua& etouq) qp llacnuf) ((lambda (q) (setf q (quote (lambda (q) (labels ((rev (x) (etypecase x (cons (reverse (mapcar (function rev) x))) (symbol (intern (reverse (symbol-name x))))))) (let ((qq (copy-tree (quote (defun pq (&aux funcall pq defun nufed quote etouq (&aux etouq) (xua& etouq) qp llacnuf) ((lambda (q) (setf q (quote qq) pq (quote list) nufed (eval (list q (list (quote quote) q))))) etouq) funcall))))) (rplaca (cdaddr (caddar (cadddr qq))) q) (append qq (rev qq)))))) pq (quote list) nufed (eval (list q (list (quote quote) q))))) etouq) funcall llacnuf (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (tsil etouq) qp ((((((qq ver) qq dneppa) (q (((qq rdddac) raddac) rddadc) acalpr) (((((llacnuf (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (tsil etouq) qp (qq etouq) q ftes) (q) adbmal)) (funcall pq (quote &aux) (quote xua&) quote etouq defun nufed qp llacnuf xua&) qp nufed) etouq) eert-ypoc) qq)) tel) (((((((x eman-lobmys) esrever) nretni) lobmys) (((x (ver noitcnuf) racpam) esrever) snoc) x esacepyte) (x) ver)) slebal) (q) adbmal) etouq) q ftes) (q) adbmal)) (funcall pq (quote &aux) (quote xua&) quote etouq defun nufed qp llacnuf xua&) qp nufed)
) ; 1251

(pqp'
(defun p (&aux funcall defun nufed quote etouq (&aux etouq) (xua& etouq) p llacnuf) ((lambda (q) (setf q (quote (lambda (q) (labels ((r (x) (if (atom x) (intern (reverse (symbol-name x))) (reverse (mapcar (function r) x))))) (let ((r (copy-tree (quote (defun p (&aux funcall defun nufed quote etouq (&aux etouq) (xua& etouq) p llacnuf) ((lambda (q) (setf q (quote r) p (quote list) nufed (eval (list q (list (quote quote) q))))) etouq) funcall))))) (rplaca (cdaddr (caddar (cadddr r))) q) (append r (r r)))))) p (quote list) nufed (eval (list q (list (quote quote) q))))) etouq) funcall llacnuf (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (tsil etouq) p ((((((r r) r dneppa) (q (((r rdddac) raddac) rddadc) acalpr) (((((llacnuf (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (tsil etouq) p (r etouq) q ftes) (q) adbmal)) (funcall p (quote &aux) (quote xua&) quote etouq defun nufed llacnuf xua&) p nufed) etouq) eert-ypoc) r)) tel) (((((x (r noitcnuf) racpam) esrever) (((x eman-lobmys) esrever) nretni) (x mota) fi) (x) r)) slebal) (q) adbmal) etouq) q ftes) (q) adbmal)) (funcall p (quote &aux) (quote xua&) quote etouq defun nufed llacnuf xua&) p nufed)
) ; 1173

(pqp'
(defun p (&aux apply defun nufed quote etouq (&aux etouq) (xua& etouq) p ylppa) ((lambda (q) (setf q (quote (lambda (q) (labels ((r (x) (if (atom x) (intern (reverse (symbol-name x))) (reverse (mapcar (function r) x))))) (let ((r (copy-tree (quote (defun p (&aux apply defun nufed quote etouq (&aux etouq) (xua& etouq) p ylppa) ((lambda (q) (setf q (quote r) p (quote list) nufed (eval (list q (list (quote quote) q))))) etouq) apply))))) (rplaca (cdaddr (caddar (cadddr r))) q) (append r (r r)))))) p (quote list) nufed (eval (list q (list (quote quote) q))))) etouq) apply ylppa (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (tsil etouq) p ((((((r r) r dneppa) (q (((r rdddac) raddac) rddadc) acalpr) (((((ylppa (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (tsil etouq) p (r etouq) q ftes) (q) adbmal)) (apply p (quote &aux) (quote xua&) quote etouq defun nufed ylppa xua&) p nufed) etouq) eert-ypoc) r)) tel) (((((x (r noitcnuf) racpam) esrever) (((x eman-lobmys) esrever) nretni) (x mota) fi) (x) r)) slebal) (q) adbmal) etouq) q ftes) (q) adbmal)) (apply p (quote &aux) (quote xua&) quote etouq defun nufed ylppa xua&) p nufed)
) ; 1149

(pqp'
(defun p (&aux list defun nufed quote etouq (&aux etouq) (xua& etouq) p tsil) ((lambda (q) (setf q (quote (lambda (q) (labels ((r (x) (if (atom x) (intern (reverse (symbol-name x))) (reverse (mapcar (function r) x))))) (let ((r (copy-tree (quote (defun p (&aux list defun nufed quote etouq (&aux etouq) (xua& etouq) p tsil) ((lambda (q) (setf q (quote r) p (quote list) nufed (eval (list q (list (quote quote) q))))) etouq) list))))) (rplaca (cdaddr (caddar (cadddr r))) q) (append r (r r)))))) p (quote list) nufed (eval (list q (list (quote quote) q))))) etouq) list tsil (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (tsil etouq) p ((((((r r) r dneppa) (q (((r rdddac) raddac) rddadc) acalpr) (((((tsil (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (tsil etouq) p (r etouq) q ftes) (q) adbmal)) (list p (quote &aux) (quote xua&) quote etouq defun nufed tsil xua&) p nufed) etouq) eert-ypoc) r)) tel) (((((x (r noitcnuf) racpam) esrever) (((x eman-lobmys) esrever) nretni) (x mota) fi) (x) r)) slebal) (q) adbmal) etouq) q ftes) (q) adbmal)) (list p (quote &aux) (quote xua&) quote etouq defun nufed tsil xua&) p nufed)
) ; 1137

(pqp'
(defun p (&aux xua& list defun nufed quote etouq (&aux etouq) p tsil) ((lambda (q) (setf q (quote (lambda (q) (labels ((r (x) (if (atom x) (intern (reverse (string x))) (reverse (mapcar (function r) x))))) (let ((r (copy-tree (quote (defun p (&aux xua& list defun nufed quote etouq (&aux etouq) p tsil) ((lambda (q) (setf q (quote r) p (quote list) nufed (eval (list q (list (quote quote) q))))) etouq) list))))) (rplaca (cdaddr (caddar (cadddr r))) q) (append r (r r)))))) p (quote list) nufed (eval (list q (list (quote quote) q))))) etouq) list tsil (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (tsil etouq) p ((((((r r) r dneppa) (q (((r rdddac) raddac) rddadc) acalpr) (((((tsil (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (tsil etouq) p (r etouq) q ftes) (q) adbmal)) (list p (quote xua&) quote etouq defun nufed tsil &aux xua&) p nufed) etouq) eert-ypoc) r)) tel) (((((x (r noitcnuf) racpam) esrever) (((x gnirts) esrever) nretni) (x mota) fi) (x) r)) slebal) (q) adbmal) etouq) q ftes) (q) adbmal)) (list p (quote xua&) quote etouq defun nufed tsil &aux xua&) p nufed)
) ; 1095

(pqp'
(defun p (&aux xua& list defun nufed quote etouq (&aux etouq) p tsil) ((lambda (q) (setf q (quote (lambda (q) (labels ((r (x) (if (atom x) (intern (reverse (string x))) (reverse (mapcar (function r) x))))) (let ((r (copy-tree (quote (defun p (&aux xua& list defun nufed quote etouq (&aux etouq) p tsil) ((lambda (q) (setf q (quote r) nufed (eval (list q (list (quote quote) q))))) etouq) list))))) (rplaca (cdaddr (caddar (cadddr r))) q) (append r (r r)))))) nufed (eval (list q (list (quote quote) q))))) etouq) list tsil (quote (((((q (etouq etouq) tsil) q tsil) lave) defun ((((((r r) r dneppa) (q (((r rdddac) raddac) rddadc) acalpr) (((((tsil (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (r etouq) q ftes) (q) adbmal)) (list p (quote xua&) quote etouq defun nufed tsil &aux xua&) p nufed) etouq) eert-ypoc) r)) tel) (((((x (r noitcnuf) racpam) esrever) (((x gnirts) esrever) nretni) (x mota) fi) (x) r)) slebal) (q) adbmal) etouq) q ftes) (q) adbmal)) (list p (quote xua&) quote etouq defun nufed tsil &aux xua&) p nufed)
) ; 1035

(pqp'
(defun p (&aux list defun nufed quote etouq (xua& etouq) p tsil) ((lambda (q) (setf q (quote (lambda (q) (labels ((r (x) (if (atom x) (intern (reverse (string x))) (reverse (mapcar (function r) x))))) (let ((r (copy-tree (quote (defun p (&aux list defun nufed quote etouq (xua& etouq) p tsil) ((lambda (q) (setf q (quote r) nufed (eval (list q (list (quote quote) q))))) etouq) list))))) (rplaca (cdaddr (caddar (cadddr r))) q) (append r (r r)))))) nufed (eval (list q (list (quote quote) q))))) etouq) list tsil (quote (((((q (etouq etouq) tsil) q tsil) lave) defun ((((((r r) r dneppa) (q (((r rdddac) raddac) rddadc) acalpr) (((((tsil (quote (((((q (etouq etouq) tsil) q tsil) lave) defun (r etouq) q ftes) (q) adbmal)) (list p (quote &aux) quote etouq defun nufed tsil xua&) p nufed) etouq) eert-ypoc) r)) tel) (((((x (r noitcnuf) racpam) esrever) (((x gnirts) esrever) nretni) (x mota) fi) (x) r)) slebal) (q) adbmal) etouq) q ftes) (q) adbmal)) (list p (quote &aux) quote etouq defun nufed tsil xua&) p nufed)
) ; 1015

(pqp'
(defun p (&aux list defun nufed quote etouq (xua& etouq) p tsil) ((lambda (q) (setq q (quote (lambda (r q) (labels ((r (x) (if (atom x) (intern (reverse (string x))) (reverse (mapcar (function r) x))))) (setf r (copy-tree (quote (defun p (&aux list defun nufed quote etouq (xua& etouq) p tsil) ((lambda (q) (setq q (quote q) nufed (eval (list q q (list (quote quote) q))))) etouq) list))) (car (cdaddr (caddar (cadddr r)))) q) (append r (r r))))) nufed (eval (list q q (list (quote quote) q))))) etouq) list tsil (quote (((((q (etouq etouq) tsil) q q tsil) lave) defun (((((r r) r dneppa) (q ((((r rdddac) raddac) rddadc) rac) (((tsil (quote (((((q (etouq etouq) tsil) q q tsil) lave) defun (q etouq) q qtes) (q) adbmal)) (list p (quote &aux) quote etouq defun nufed tsil xua&) p nufed) etouq) eert-ypoc) r ftes) (((((x (r noitcnuf) racpam) esrever) (((x gnirts) esrever) nretni) (x mota) fi) (x) r)) slebal) (q r) adbmal) etouq) q qtes) (q) adbmal)) (list p (quote &aux) quote etouq defun nufed tsil xua&) p nufed)
) ; 1015

(pqp'
(defun p (&aux list defun nufed quote etouq (xua& etouq) p tsil) ((lambda (q) (setq q (quote (lambda (r q) (labels ((r (x) (if (atom x) (intern (reverse (string x))) (reverse (mapcar (function r) x))))) (setf r (copy-tree (quote (defun p (&aux list defun nufed quote etouq (xua& etouq) p tsil) ((lambda (q) (setq q (quote q) nufed (eval (list q q (list (quote quote) q))))) etouq) list))) (car (cdaddr (caddar (cadddr r)))) q) (nconc r (r r))))) nufed (eval (list q q (list (quote quote) q))))) etouq) list tsil (quote (((((q (etouq etouq) tsil) q q tsil) lave) defun (((((r r) r cnocn) (q ((((r rdddac) raddac) rddadc) rac) (((tsil (quote (((((q (etouq etouq) tsil) q q tsil) lave) defun (q etouq) q qtes) (q) adbmal)) (list p (quote &aux) quote etouq defun nufed tsil xua&) p nufed) etouq) eert-ypoc) r ftes) (((((x (r noitcnuf) racpam) esrever) (((x gnirts) esrever) nretni) (x mota) fi) (x) r)) slebal) (q r) adbmal) etouq) q qtes) (q) adbmal)) (list p (quote &aux) quote etouq defun nufed tsil xua&) p nufed)
) ; 1013

(pqp'
(defun p (&aux defun nufed quote etouq (xua& etouq) p tsil) ((lambda (q) (setq q (quote (lambda (r q) (labels ((r (x) (if (atom x) (intern (reverse (string x))) (reverse (mapcar (function r) x))))) (setf r (copy-tree (quote (defun p (&aux defun nufed quote etouq (xua& etouq) p tsil) ((lambda (q) (setq q (quote q) tsil q nufed (eval (list q q (list (quote quote) q))))) etouq)))) (car (cdaddr (caddar (cadddr r)))) q) (nconc r (r r))))) tsil q nufed (eval (list q q (list (quote quote) q))))) etouq) (quote (((((q (etouq etouq) tsil) q q tsil) lave) defun q list (((((r r) r cnocn) (q ((((r rdddac) raddac) rddadc) rac) ((((quote (((((q (etouq etouq) tsil) q q tsil) lave) defun q list (q etouq) q qtes) (q) adbmal)) (list p (quote &aux) quote etouq defun nufed xua&) p nufed) etouq) eert-ypoc) r ftes) (((((x (r noitcnuf) racpam) esrever) (((x gnirts) esrever) nretni) (x mota) fi) (x) r)) slebal) (q r) adbmal) etouq) q qtes) (q) adbmal)) (list p (quote &aux) quote etouq defun nufed xua&) p nufed)
) ; 1001

(pqp'
(defun p (&aux defun nufed quote etouq (xua& etouq) p tsil) ((lambda (q) (setq q (quote (lambda (q) (labels ((r (x) (if (atom x) (intern (reverse (string x))) (reverse (mapcar (function r) x)))) (q (q) (subst q (eq q q) (quote (defun p (&aux defun nufed quote etouq (xua& etouq) p tsil) ((lambda (q) (setq q (quote t) tsil q nufed (eval (list q (list (quote quote) q))))) etouq)))))) (nconc (q q) (r (q q)))))) tsil q nufed (eval (list q (list (quote quote) q))))) etouq) (quote (((((q (etouq etouq) tsil) q tsil) lave) defun q list ((((((q q) r) (q q) cnocn) ((((((quote (((((q (etouq etouq) tsil) q tsil) lave) defun q list (t etouq) q qtes) (q) adbmal)) (list p (quote &aux) quote etouq defun nufed xua&) p nufed) etouq) (q q qe) q tsbus) (q) q) ((((x (r noitcnuf) racpam) esrever) (((x gnirts) esrever) nretni) (x mota) fi) (x) r)) slebal) (q) adbmal) etouq) q qtes) (q) adbmal)) (list p (quote &aux) quote etouq defun nufed xua&) p nufed)
) ; 943

;; Precursor to the solution... use it with #. !
(let((q'(lambda(q)(labels((r(x)(if(atom x)(intern(reverse(string x)))(
reverse(mapcar #'r x))))(q(q)(subst q(eq q q)'(defun p(&aux defun nufed
quote etouq(xua& etouq)p tsil)((lambda(q)(setq q 't tsil q nufed(eval(
list q(list'quote q)))))etouq)))))(nconc(q q)(r(q q)))))))(eval`(,q',q)))
