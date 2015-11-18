":" ; exec cl-launch -s fare-puzzles/zebra

(uiop:define-package :fare-puzzles/misc/zebra
  (:mix :fare-utils :uiop :alexandria :common-lisp))
(in-package :fare-puzzles/misc/zebra)


(declaim (optimize (speed 2) (safety 3) (debug 3)))

#| "
2001-10-12
This puzzle was sent to my sister by email.
clisp -i zebra.lisp -x '(quit)'>&z.out

A google search revealed that it was already available at following URLs:
	http://www.rudstrand.com/einstein/puzzle.html
	http://www.justriddlesandmore.com/questiontwo.html
The elegant demonstration in
	http://www.evillama.com/whatsgoingon/games/einstein.htm
suggests that individual symbols rather than whole categories
ought to be used as choice-points, which could simplify the whole thing.
	http://www.clairv.com/MLton/benchmarks/zebra.sml
" |#

(defparameter zebra-problem
  '((categories
     ((house (1 2 3 4 5))
      (color (blue green yellow red white))
      (nationality (English Swede Dane German Norvegian))
      (cigarette (Pall_Mall DunHill Blends BlueMaster Prince))
      (drink (milk tea beer coffee water))
      (pet (zebra cat dog horse bird))))
    (constraints
     ((c red English)
      (c Swede dog)
      (c Dane tea)
      (left-right green white)
      (c green coffee)
      (c Pall_Mall bird)
      (c yellow DunHill)
      (c 3 milk)
      (c 1 Norvegian)
      (neighbours Blends cat)
      (neighbours horse DunHill)
      (c BlueMaster beer)
      (c German Prince)
      (neighbours Norvegian blue)
      (neighbours Blends water)))))

(defparameter problem-categories (second (first zebra-problem)))
(defparameter problem-constraints (second (second zebra-problem)))
(defparameter num-categories (length problem-categories))
(defconstant num-atoms 5)
(defparameter num-constraints (length problem-constraints))
(defparameter list-0-4 '(0 1 2 3 4))
(defparameter solutions-found '())
(defun cat-to-num (cat)
  (position cat problem-categories :key #'first))
(defun num-to-cat (num)
  (first (nth num problem-categories)))
(defparameter nationality-num (cat-to-num 'nationality))
(defun make-coord (n m) (+ m (* n num-atoms)))
(defun coord-catnum (coord) (floor coord num-atoms))
(defun coord-atmnum (coord) (mod coord num-atoms))
(defparameter atom-to-coord-hash (make-hash-table :size 30))
(defun init-atom-to-coord ()
  (loop for cat in problem-categories
    for catnum from 0 do
    (loop with atoms = (second cat)
      for atom in atoms
      for atmnum from 0 do
      (setf (gethash atom atom-to-coord-hash) (make-coord catnum atmnum)))))
(init-atom-to-coord)
(defun atom-to-coord (atom)
  (or (gethash atom atom-to-coord-hash)
      (error "Atom ~A not known." atom)))
(defun coord-to-atom (coord)
  (let* ((catnum (coord-catnum coord))
	 (category (nth catnum problem-categories))
	 (atmnum (coord-atmnum coord))
	 (atoms (second category)))
    (nth atmnum atoms)))
(defparameter zebra-coord (atom-to-coord 'zebra))
(defun readable-correspondances (correspondances)
  (loop
    for ix across correspondances
    collect (if ix (coord-to-atom ix) '_)))
(defun corresponding-atom-num (catnum correspondances)
  (aref correspondances catnum))
(defun make-atom-correspondances ()
  (make-array '(30)
   :initial-contents
   (loop for catnum from 0 to (1- num-categories) nconc
     (loop for atmnum from 0 to (1- num-atoms)
       for c = (make-array num-categories :initial-element nil)
       do (setf (svref c catnum) (make-coord catnum atmnum))
       collect c))))
(defun atoms-in-category (catnum)
  (let* ((lo (* catnum num-atoms))
	 (hi (+ lo num-atoms -1)))
    (list-of-integers lo hi)))
(defun make-category-choices ()
  (loop for c1 from 0 to (- num-categories 2) nconc
    (loop for c2 from (1+ c1) to (1- num-categories) collect
      (list 'category c1 c2 num-atoms
	    (atoms-in-category c1) (atoms-in-category c2)))))
(defstruct (universe (:copier nil))
  (atom-correspondances (make-atom-correspondances))
  ;(category-choices (make-category-choices))
  (constraints nil))
(defun copy-universe (universe)
  (make-universe
   :atom-correspondances (copy-array (universe-atom-correspondances universe))
;  :category-choices (universe-category-choices universe)
   :constraints (universe-constraints universe)))
(defmacro try-search (&body body)
  `(catch 'search-failed ,@body))
(defparameter depth 0)
(defun spaces (n)
  (make-string n :initial-element #\Space))
(defun message (&rest rest)
  (format t "~&~A" (spaces depth))
  (apply #'format t rest))
(defun fail! ()
  ;;(message "X impossible!")
  (throw 'search-failed nil))
(defun fact (n)
  "factorial of positive integer n"
  (loop with product = 1
    for i from n downto 2 do (setf product (* product i))
    finally (return product)))
(defun smallfact (n)
  (aref #(1 1 2 6 24 120) n))
(defun solution! (universe)
  (push universe solutions-found)
  #|(show-solution universe)|#)
(defun show-solution (universe)
  (message "Found solution with nationality ~A:"
	  (coord-to-atom
	   (corresponding-atom-num
	    nationality-num
	    (get-atom-correspondances universe zebra-coord))))
  (show-universe universe))
;(message " ~W" (readable-choice universe list-0-4))
(defun add-constraint! (universe constraint)
  (ecase (first constraint)
    (c (add-c-constraint! universe constraint))
    ((neighbours left-right) (add-to-constraint-list! universe constraint))))
(defun add-c-constraint! (universe constraint)
  (let ((a1 (atom-to-coord (second constraint)))
	(a2 (atom-to-coord (third constraint))))
    (if (can-merge-atoms-p universe a1 a2)
	(merge-atoms! universe a1 a2)
      (fail!))))
(defun add-to-constraint-list! (universe constraint)
  (let ((kind (first constraint))
	(c1 (atom-to-coord (second constraint)))
	(c2 (atom-to-coord (third constraint))))
    (push (list kind c1 c2) (universe-constraints universe))))
(defun can-merge-correspondances-p (c1 c2)
  (or (eq c1 c2)
      (loop for x1 across c1 for x2 across c2
	never (and x1 x2 (not (eql x1 x2))))))
(defun test-can-merge-atoms-p (universe a1 a2)
  (can-merge-atoms-p universe (atom-to-coord a1) (atom-to-coord a2)))
(defun can-merge-atoms-p (universe a1 a2)
  (can-merge-correspondances-p
   (get-atom-correspondances universe a1)
   (get-atom-correspondances universe a2)))
(defparameter threshhold 2)
(defmacro atom-correspondances (universe coord)
  `(aref (universe-atom-correspondances ,universe) ,coord))
(defun forward-pointer-p (c) (numberp c))
(defun forward-pointer-coord (c) c)
(defun make-forward-pointer (c) c)
(defun get-atom-head (universe coord)
  (let ((c (atom-correspondances universe coord)))
    (if (forward-pointer-p c) (forward-pointer-coord c) coord)))
(defun get-atom-correspondances (universe coord)
  (let ((c (atom-correspondances universe coord)))
    (if (forward-pointer-p c)
	(atom-correspondances universe (forward-pointer-coord c))
      c)))
(defun set-atom-correspondances (universe coord new-coord)
  (setf (atom-correspondances universe coord)
	(make-forward-pointer new-coord)))
(defparameter *universe* nil)
(defun init-universe ()
  (let ((universe (make-universe)))
    (unless
	(try-search
	 (mapcar #'(lambda (c) (add-constraint! universe c))
		 problem-constraints)
	 t)
      (error "Universe proved inconsistent at init time."))
    (setf *universe* (copy-universe universe))
    ;(show-universe universe)
    universe))
(defun main-search-f (&key repeat)
  (loop :repeat (or repeat 1) :do
    (setf solutions-found '())
    (try-search
      (search-space (init-universe))))
  (format t "~&Found ~D solution~:*~P:~%" (length solutions-found))
  (map () #'show-solution solutions-found)
  (terpri))

(defun search-space (universe)
  (let ((*universe* universe)) ; debugging
  (if (successp universe)
      (solution! universe)
    (multiple-value-bind (choice-point choice-hints)
	(find-choice-point universe)
    (branch-search universe choice-point choice-hints)))))
(defun find-choice-point (universe)
  (flet
     ((estimate-point (choice-point)
       (multiple-value-bind
	   (choice-entropy choice-hints)
	   (estimate-choices universe choice-point)
	 (if (< choice-entropy threshhold)
	     (return-from find-choice-point
	       (values choice-point choice-hints))
	   (list choice-entropy choice-point choice-hints)))))
    (let ((choice-point-estimates
	   (for-each-choice-point universe #'estimate-point)))
      (if (null choice-point-estimates)
	  (progn #|(message "no possible choice")|# (fail!))
	(let* ((best-point-estimate
		(extremum choice-point-estimates #'< :key #'first))
	       (choice-point (second best-point-estimate))
	       (choice-hints (third best-point-estimate)))
	  (values choice-point choice-hints))))))
(defun readable-choice-point (universe x)
  (case (first x)
    (category (list 'category
		    (num-to-cat (second x))
		    (num-to-cat (third x))
		    (fourth x)
		    (readable-choice universe (fifth x))
		    (readable-choice universe (sixth x))))
    (t (readable-choice universe x))))
(defun readable-choice (universe x)
  (cons-tree-map
   #'(lambda (a)
       (cond ((numberp a) (readable-correspondances
			   (get-atom-correspondances universe a)))
	     (t a)))
   x))
(defun readable-universe (universe)
  (list
   (loop for x across (universe-atom-correspondances universe)
     unless (forward-pointer-p x)
     collect (readable-correspondances x))
;   (loop for x in (universe-category-choices universe)
;     collect (readable-choice-point universe x))
   (loop for x in (universe-constraints universe)
     collect (readable-choice universe x))))
(defun show-universe (universe)
  ;(message "~W" universe)
  (message "~W" (readable-universe universe)))
(defun branch-search (universe choice-point choice-hints)
  (let ((choice-list (or choice-hints (choices universe choice-point))))
    (case (length choice-list)
      (0 (fail!))
      (1 #|(message "Forced choice: ~A is ~A"
		  (readable-choice-point universe choice-point)
		  (readable-choice universe (first choice-list)))|#
	 (do-choice universe choice-point (first choice-list)))
      (t #|(message "Choice (~A): ~A among ~A"
		  (length choice-list)
		  (readable-choice-point universe choice-point)
		  (readable-choice universe choice-list))|#
         (incf depth)
	 (dolist (choice choice-list)
	   #|(message "Trying ~A as ~A"
		    (readable-choice-point universe choice-point)
		    (readable-choice universe choice))|#
	   (try-choice universe choice-point choice))
	 (decf depth)))))
(defun try-choice (universe choice-point choice)
  (try-search
    (let ((new-universe (copy-universe universe)))
      (do-choice new-universe choice-point choice))))
(defun successp (universe)
  (and (null (universe-constraints universe))
       (loop for n from 29 downto 5
	 always (forward-pointer-p (atom-correspondances universe n)))))
(defun merge-correspondances (c1 c2)
  (make-array num-categories :initial-contents
	      (loop for x1 across c1 for x2 across c2 collect (or x1 x2))))
(defun merge-atoms! (universe a1 a2)
  ;(show-universe universe)
  (let ((h1 (get-atom-head universe a1))
	(h2 (get-atom-head universe a2)))
    (if (= h1 h2) (return-from merge-atoms!))
    (if (< h2 h1) (rotatef h1 h2))
    (let* ((c1 (atom-correspondances universe h1))
	   (c2 (atom-correspondances universe h2))
	   (mc (merge-correspondances c1 c2)))
      (set-atom-correspondances universe h1 mc)
      (loop for x2 across c2 when x2
	do (set-atom-correspondances universe x2 h1)))))
(defun unknown-correspondance (universe cat1 cat2)
  (loop for atom in (atoms-in-category cat2)
    when (null (corresponding-atom-num
		cat1 (get-atom-correspondances universe atom)))
    collect atom))
(defun for-each-choice-point (universe fun)
  (let ((choices (mapcar fun (universe-constraints universe))))
    (flet ((f (x) (push (funcall fun x) choices)))
      (for-each-category-choice universe #'f))
    choices))
(defun for-each-category-choice (universe fun)
  (loop with cat1 = 0
    for cat2 from 1 to 5
    for unknown2 = (unknown-correspondance universe cat1 cat2)
    when unknown2 do
    (funcall fun (list 'category cat1 cat2 (length unknown2)
		       (unknown-correspondance universe cat2 cat1) unknown2))))
(defun estimate-choices (universe choice-point)
  (ecase (first choice-point)
    ((category)
     (estimate-category-choices choice-point))
    ((left-right neighbours)
     (estimate-constraint-choices universe choice-point))))
(defun estimate-category-choices (choice-point)
  (values (smallfact (fourth choice-point)) nil))
(defun left-right-choices (universe constraint)
  (let ((a1 (second constraint)) (a2 (third constraint)))
    (loop for n from 0 to 3 for m = (1+ n)
      when (and (can-merge-atoms-p universe n a1)
		(can-merge-atoms-p universe m a2))
      collect (list n m))))
(defun neighbours-choices (universe constraint)
  (let ((a1 (second constraint)) (a2 (third constraint)))
    #+XXX (message "neighbours-choices ~A ~A"
	     (readable-choice universe a1) (readable-choice universe a2))
    (loop with choices = nil
      for n from 0 to 3 for m = (1+ n) do
      (progn
	(when (and (can-merge-atoms-p universe n a1)
		   (can-merge-atoms-p universe m a2))
	  (push (list n m) choices))
	(when (and (can-merge-atoms-p universe n a2)
		   (can-merge-atoms-p universe m a1))
	  (push (list m n) choices)))
      finally (return choices))))
(defun estimate-constraint-choices (universe constraint)
  (let ((choices (constraint-choices universe constraint)))
    (if (null choices) (fail!)
      (values (length choices) choices))))
(defun constraint-choices (universe constraint)
  (ecase (first constraint)
    (neighbours (neighbours-choices universe constraint))
    (left-right (left-right-choices universe constraint))))
(defun choices (universe choice-point)
  (ecase (first choice-point)
    ((category)
     (category-choices choice-point))
    ((left-right neighbours)
     (constraint-choices universe choice-point))))
(defun category-choices (choice-point)
  (let ((l1 (fifth choice-point)) (l2 (sixth choice-point)))
    (permutations l1 l2)))
(defun permutations (l1 l2)
  (if (null l1) '(nil)
    (loop with x1 = (car l1) for x2 in l2 nconc
	(mapcar #'(lambda (p) (cons (list x1 x2) p))
		(permutations (cdr l1) (remove x2 l2))))))
(defun do-choice (universe choice-point choice)
  (ecase (first choice-point)
    ((category)
     (do-category-choice universe choice))
    ((left-right neighbours)
     (do-constraint-choice universe choice-point choice)))
  (search-space universe))
(defun do-constraint-choice (universe constraint choice)
  (let ((a1 (second constraint)) (a2 (third constraint))
	(n1 (first choice)) (n2 (second choice)))
    (merge-atoms! universe a1 n1)
    (merge-atoms! universe a2 n2)
    (setf (universe-constraints universe)
	  (remove constraint (universe-constraints universe)))))
(defun do-category-choice (universe mapping)
  (loop
    for pair in mapping
    for x = (first pair)
    for y = (second pair)
    do (merge-atoms! universe x y)))

(time (main-search-f :repeat 100))

;(fare-utils:quit)

#| "
Only solution (rot13)

Sbhaq fbyhgvba jvgu angvbanyvgl TREZNA:
(((1 LRYYBJ ABEIRTVNA QHAUVYY JNGRE PNG)
  (2 OYHR QNAR OYRAQF GRN UBEFR)
  (3 ERQ RATYVFU CNYY_ZNYY ZVYX OVEQ)
  (4 TERRA TREZNA CEVAPR PBSSRR MROEN)
  (5 JUVGR FJRQR OYHRZNFGRE ORRE QBT))
 AVY)

* Mettre l'espace en forme normale:
 remplacer chaque atome par l'atome correspondant connu de type le plus petit.
* simplifier voisin quand l'indice est connu
* Enlever les expressions qui n'apporteront plus rien par rapport aux données simplifiées qui en découlent.
* Heuristique: chercher la ``variable la plus contrainte parmi les inconnues sur lesquelles une information est disponible qui permet un choix'' (i.e. la correspondance encore indeterminée entre une constante et une variable d'une autre catégorie, la où la catégorie est la plus petite à condition qu'il y ait des propositions portant sur cette catégorie.)

* représentation plus contrainte: 5 permutations, d'où espace de taille 120**5.
 Or, division par 5 de l'espace total par équation. Soit, pour 15 équations,
 un espace divisé 5**15 fois -- à peu près le même ordre de grandeur
* variables (6*5*5), chacune d'espace 5 -- donc espace de taille 5**150
 au départ. Mais chaque égalité réduit l'espace des variables d'autant
 (toutes les variables associées à l'autre terme de l'égalité)
 et restreint l'espace de chaque autre variable dans le groupe.

* Univers de recherche, v1:
 = pour chaque couple de catégories (15), la liste des atomes non alloués.
 = pour chaque atome (25), un lien vers un vecteur des correspondances connues
  pour cet atome (ces listes sont fusionnées quand deux atomes correspondent).
 = un ensemble de contraintes de voisinage à simplifier.
* Le fait que les contraintes soient de voisinage implique que la catégorie
 ``position'' est privilégiée.

* Univers de recherche, v2:
 = pour chaque atome (30), un lien vers un vecteur des correspondances connues
  pour cet atome (ces listes sont fusionnées quand deux atomes correspondent).
  Un tableau des atomes compatibles.
  Entropie locale: log2(nombre d'atomes compatibles)b.
 = pour chaque couple de catégories (15), la liste des atomes non alloués.
  Entropie locale: log2(fact(la taille des listes))b.
  [inutile de calculer l'entropie locale si on à celle des atomes]
 = un ensemble de contraintes de voisinage à simplifier.
  Entropie locale:
   log2(card(paires compatibles avec la contrainte))
* Propagation de contrainte:
 = Fusionner les atomes si compatibles. Sinon, couper l'arbre de recherche.
 = si un couple de catégories n'admet plus qu'une seule solution,
  l'adopter de suite.
 = si la fusion de deux symboles implique une impossibilité,
  couper la branche de l'arbre de recherche
* Algorithme:
 tant que pas de modèle satisfaisant (i.e. plus de contraintes externes,
 contraintes internes complètement résolues),
  = propager les contraintes externes simples
  = déterminer un ensembles d'objets dont les branchements diminuent
   strictement l'espace de recherche.
  = pour chacun de ces objets, dans l'ordre de préférence précédent,
   calculer l'entropie locale
   (différence entre coût de de branchement et information apportée).
   Raccourci si l'entropie est en-dessous du seuil, propager et recommencer.
  = Si aucun raccourci n'a été pris, trier les objets, choisir l'objet de plus
   petite entropie, et brancher dessus.
" |#
