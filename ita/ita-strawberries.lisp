#|;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
':' 1 ; cat <<'Solving a simple problem from ITA'>> /dev/null
|#
(defvar *version* "1.22")
#| "
Problem description

originally here: http://www.itasoftware.com/careers/eng/job1.php
archived here: http://www.itasoftware.com/careers/programmers-archive.php
auxiliary file: http://www.itasoftware.com/careers/eng/rectangles.txt

   _Strawberry Fields_

   Strawberries are growing in a rectangular field of length and width at
   most 50. You want to build greenhouses to enclose the strawberries.
   Greenhouses are rectangular, axis-aligned with the field (i.e., not
   diagonal), and may not overlap. The cost of each greenhouse is $10
   plus $1 per unit of area covered.

   Write a program that chooses the best number of greenhouses to build,
   and their locations, so as to enclose all the strawberries as cheaply
   as possible. Heuristic solutions that may not always produce the
   lowest possible cost will be accepted: seek a reasonable tradeoff of
   efficiency and optimality.

   Your program must read a small integer 1 =< N =< 10 representing the
   maximum number of greenhouses to consider, and a matrix representation
   of the field, in which the '@' symbol represents a strawberry. Output
   must be a copy of the original matrix with letters used to represent
   greenhouses, preceded by the covering's cost. Here is an example
   input-output pair:

                                Input Output
4
..@@@@@...............
..@@@@@@........@@@...
.....@@@@@......@@@...
.......@@@@@@@@@@@@...
.........@@@@@........
.........@@@@@........

90
..AAAAAAAA............
..AAAAAAAA....CCCCC...
..AAAAAAAA....CCCCC...
.......BBBBBBBCCCCC...
.......BBBBBBB........
.......BBBBBBB........

   In this example, the solution cost of $90 is computed as (10+8*3) +
   (10+7*3) + (10+5*3).

   Run your program on the 9 sample inputs found in this file and report
   the total cost of the 9 solutions found by your program, as well as
   each individual solution.

" "
This program provides an exact solution to the problem,
that solves all the proposed examples in finite time.
See comments and explanations about this program at the end of the file.

" |#

(in-package :cl-user)

(proclaim '(optimize (speed 3) (space 0) (safety 3) (debug 3)))
#+debug (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3)))
#-debug (declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))

;;; Paths used for test examples
(defvar *ita-dir* "/home/fare/ita/")
(defvar *lisp-dir* "/home/fare/fare/lisp/")


(defvar *problem* nil
  "An all-encompassing representation of the problem at hand.") ; XXX - unused
(defvar *field-lines* () "The initial contents of the problem field.")
(defvar *max-greenhouses* 0 "Maximal number of allowed greenhouses.")
(defvar *rows* 0 "Number of rows in the problem field.")
(defvar *columns* 0 "Number of rows in the problem field.")
(defvar *xx* 0 "Number of rows in the simplified field.")
(defvar *yy* 0 "Number of columns in the simplified field.")
(defvar *field* (make-array '(0 0) :element-type 'base-char)
  "The simplified array.")
(defvar *x-row* ()
  "Translation table from simplified field row to original field row.")
(defvar *y-column* ()
  "Translation table from simplified field column to original field column.")
(defvar *bounding-box* nil
  "A rectangle that delimits the used portion of the field.")
(defvar *cost* nil
  "The cost of the best known solution.")
;(defvar *solutions* () "A list of solutions to consider.")
(defvar *solution* () "The best known solution.")
(defvar *area* 0 "Total area occupied by strawberry spots.")
(defvar *remaining-area* 0 "Area occupied by non-covered strawberry spots.")
(defvar *concepts* ()
  "List of (remaining) concepts: elementary rectangles to merge into houses")
(defvar *current-cost* nil "Current cost for rectangles.")
(defvar *n-concepts* 0 "number of (remaining) concepts.")
(defvar *rectangles* #() "vector of rectangles")
(defvar *n-rectangles* 0 "number of considered rectangles")
(defvar *max-rectangle* 0 "maximum index for rectangles")
(defvar *empty-rectangles* 0 "number of empty retangles")
(defvar *n* 0 "requested number of rectangles")
(defvar *max-interest* 1e6 "maximal interest for a concept to consider.")
(defvar *house-names* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  "Names for greenhouses.")
(defvar *concept-names* "abcdefghijklmnopqrstuvwxyz0123456789~!@#$%^&*()_+[]|:\"<>?`-=[]\\;',/ZYXWVUTSRQPONMLK"
  "Names for concepts.")
(defvar *search-strategy* nil
  "Which search strategy we are using to solve the problem.")

(defstruct (rectangle (:conc-name $))
  top bottom left right)
(defun rectangle->cost (rectangle)
  (+ 10
     (* (1+ (- ($bottom rectangle) ($top rectangle)))
	(1+ (- ($right rectangle) ($left rectangle))))))
(defun rectangles->cost (rlist)
  (reduce #'+ rlist :key 'rectangle->cost :initial-value 0))
(defun list-rectangles (&rest rectangles)
  (remove nil rectangles))
(defun cons-rectangle (maybe-nil list)
  (if maybe-nil (cons maybe-nil list) list))
(defun xy-rectangle->area (rectangle)
  (* (- (aref *x-row* (1+ ($bottom rectangle)))
	(aref *x-row* ($top rectangle)))
     (- (aref *y-column* (1+ ($right rectangle)))
	(aref *y-column* ($left rectangle)))))
(defun xy->area (x y)
  (* (- (aref *x-row* (1+ x)) (aref *x-row* x))
     (- (aref *y-column* (1+ y)) (aref *y-column* y))))
(defun xy-rectangle->cost (rectangle)
  (+ 10 (xy-rectangle->area rectangle)))
(defun xy-rectangles->cost (rlist)
  (reduce #'+ rlist :key 'xy-rectangle->cost :initial-value 0))

(defstruct (arectangle (:include rectangle) (:conc-name $))
  (area 0))
(defun make-xy-arectangle (&key top bottom left right)
  (let* ((arectangle (make-arectangle
		      :top top :bottom bottom :left left :right right))
	 (area (xy-rectangle->area arectangle)))
    (setf ($area arectangle) area)
    arectangle))
(defun $x (concept) ($top concept))
(defun $y (concept) ($left concept))
(defun make-concept (&key x y)
  (make-arectangle :top x :bottom x :left y :right y
		:area (xy->area x y)))
(defun arectangle-cost (A)
  (+ 10 ($area A)))

(defun read-max-greenhouses (&optional i)
  (let ((max (read i)))
    (unless (typep max '(integer 1 10))
      (error "Invalid max number of greenhouses, not integer from 1 to 10"))
    (setq *max-greenhouses* max)))
(defun make-field-from-lines ()
  (make-array (list *rows* *columns*) :element-type 'base-char
			    :initial-contents *field-lines*))
(defun read-field (&optional i)
  (setf *field-lines*
	(loop for l = (read-line i nil)
	  until (or (null l) (equal l ""))
	  collect l))
  (unless (consp *field-lines*) (error "Malformed input: Empty field"))
  (setf *rows* (length *field-lines*)
	*columns* (length (car *field-lines*)))
  (unless (<= *rows* 50) (error "Field too big, height more than 50."))
  (unless (<= *columns* 50) (error "Field too big, width more than 50."))
  (unless (every #'(lambda (c) (= *columns* (length c))) *field-lines*)
    (error "lines do not have the same size~%~W" *field-lines*))
  (setf *field* (make-field-from-lines)))
(defun iota (min max) (loop for i from min to max collect i))
(defun read-problem (&optional i)
  (read-max-greenhouses i)
  (read-field i))


(defun translate-coord-min (x x-row)
  (aref x-row x))
(defun translate-coord-max (x x-row)
  (1- (aref x-row (1+ x))))
(defun translate-rectangle (rectangle
			    &optional (x-row *x-row*) (y-column *y-column*))
  (make-rectangle :top (translate-coord-min ($top rectangle) x-row)
		  :bottom (translate-coord-max ($bottom rectangle) x-row)
		  :left (translate-coord-min ($left rectangle) y-column)
		  :right (translate-coord-max ($right rectangle) y-column)))
(defun translate-rectangles (rectangles)
  (mapcar 'translate-rectangle rectangles))

#|
Attempt to take advantage of the symmetry in simple cases.
Aborted because the more advanced heuristic
already keep such things factored in a tiny spot.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *symmetricals*
  '(x y *xx* *yy* row column *x-row* *y-column*
    empty-cell-p cell-empty-p empty-row empty-column
    ))
); eval-when
|#

(defun print-field (&optional (o t) (field *field*))
  (loop for i below *rows* do
    (progn
      (format o "~A~%"
	      (make-array (list *columns*) :element-type 'base-char
			  :displaced-to field
			  :displaced-index-offset (* i *columns*))))))
(defun print-solution (&optional (o t) (field *field*))
  (format o "~A~%" *cost*)
  (print-field o field))

(defun empty-cell-p (field x y)
  (eql (aref field x y)
	     #\.))
;	     (translate-coord-min x *x-row*)
;	     (translate-coord-min y *y-column*))

;(defun cell-empty-p (field y x)
;  (empty-cell-p field x y))

(defun empty-row (field row min-y max-y)
  (loop for column from min-y to max-y
    always (empty-cell-p field row column)))
(defun empty-column (field column min-x max-x)
  (loop for row from min-x to max-x
    always (empty-cell-p field row column)))

;;; remove lines and rows of pure whitespace around strawberries,
;;; to find the best greenhouse that covers all the strawberries in the area.
;;; return nil if no strawberries in the area, a rectangle if there are
(defun best-single-greenhouse
  (&key (min-x 0) (max-x *xx*)
	(min-y 0) (max-y *yy*)
	(field *field*))
  (loop while (and (<= min-x max-x)
		   (empty-row field min-x min-y max-y))
    do (incf min-x))
  (loop while (and (<= min-x max-x)
		   (empty-row field max-x min-y max-y))
    do (decf max-x))
  (when (<= min-x max-x)
    (loop while (and (<= min-y max-y)
		     (empty-column field min-y min-x max-x))
      do (incf min-y))
    (loop while (and (<= min-y max-y)
		     (empty-column field max-y min-x max-x))
      do (decf max-y))
    (make-rectangle :top min-x :bottom max-x
		    :left min-y :right max-y)))

(defun find-best-single-greenhouse ()
  (propose-solution (cons-rectangle (best-single-greenhouse) nil)))

#|
(defmacro with-solutions (&body body)
  `(let ((*solutions* ()))
     ,@body
     (when *solutions*
       (find-mostest *solutions* #'< :key 'xy-rectangles->cost))))
(defun propose-solution (solution)
  (when solution (push solution *solutions*)))
|#

(defmacro with-solutions (&body body)
  `(let (*solution* *cost*)
     ,@body
     *solution*))
(defun propose-solution (solution
			 &aux (cost (xy-rectangles->cost solution)))
  (when (and solution (or (not *cost*) (< cost *cost*)))
    (setf *solution* solution *cost* cost)))

(defun propose-sensible-greenhouse-pairs-horizontal
  (&key (min-x 0) (max-x *xx*)
	(min-y 0) (max-y *yy*)
	(field *field*))
  (loop for row from min-x below max-x do
    (propose-solution
     (list-rectangles
      (best-single-greenhouse :min-x min-x :max-x row
			      :min-y min-y
			      :max-y max-y :field field)
      (best-single-greenhouse :min-x (1+ row) :max-x max-x
			      :min-y min-y
			      :max-y max-y :field field)))))
(defun propose-sensible-greenhouse-pairs-vertical
  (&key (min-x 0) (max-x *xx*)
	(min-y 0) (max-y *yy*)
	(field *field*))
  (loop for column from min-y below max-y do
    (propose-solution
     (list-rectangles
      (best-single-greenhouse :min-x min-x :max-x max-x
			      :min-y min-y
			      :max-y column :field field)
      (best-single-greenhouse :min-x min-x :max-x max-x
			      :min-y (1+ column)
			      :max-y max-y :field field)))))
(defun propose-sensible-greenhouse-pairs
  (&key (min-x 0) (max-x *xx*)
	(min-y 0) (max-y *yy*)
	(field *field*))
  (propose-sensible-greenhouse-pairs-horizontal
    :min-x min-x :max-x max-x
    :min-y min-y :max-y max-y :field field)
  (propose-sensible-greenhouse-pairs-vertical
    :min-x min-x :max-x max-x
    :min-y min-y :max-y max-y :field field))

(defun best-greenhouse-pair-vertical
  (&key (min-x 0) (max-x *xx*)
	(min-y 0) (max-y *yy*)
	(field *field*))
  (with-solutions
   (propose-sensible-greenhouse-pairs-vertical
    :min-x min-x :max-x max-x
    :min-y min-y :max-y max-y :field field)))
(defun best-greenhouse-pair-horizontal
  (&key (min-x 0) (max-x *xx*)
	(min-y 0) (max-y *yy*)
	(field *field*))
  (with-solutions
   (propose-sensible-greenhouse-pairs-horizontal
    :min-x min-x :max-x max-x
    :min-y min-y :max-y max-y :field field)))
(defun best-greenhouse-pair
  (&key (min-x 0) (max-x *xx*)
	(min-y 0) (max-y *yy*)
	(field *field*))
  (with-solutions
   (propose-sensible-greenhouse-pairs
    :min-x min-x :max-x max-x
    :min-y min-y :max-y max-y :field field)))

(defun find-best-greenhouse-pair ()
  (propose-solution (best-greenhouse-pair
		     :min-x 0
		     :max-x *xx*
		     :min-y 0
		     :max-y *yy*)))

(defun propose-sensible-greenhouse-triples
  (&key (min-x 0) (max-x *xx*)
	(min-y 0) (max-y *yy*)
	(field *field*))
  (loop for row from min-x below max-x do
    (propose-solution
     (cons-rectangle
      (best-single-greenhouse :min-x min-x :max-x row
			      :min-y min-y
			      :max-y max-y :field field)
      (best-greenhouse-pair :min-x (1+ row) :max-x max-x
			    :min-y min-y
			    :max-y max-y :field field)))
    (propose-solution
     (cons-rectangle
      (best-single-greenhouse :min-x (1+ row) :max-x max-x
			      :min-y min-y
			      :max-y max-y :field field)
      (best-greenhouse-pair-vertical :min-x min-x :max-x row
			   :min-y min-y
			   :max-y max-y :field field))))
  (loop for column from min-y below max-y do
    (propose-solution
     (cons-rectangle
      (best-single-greenhouse :min-x min-x :max-x max-x
			      :min-y min-y
			      :max-y column :field field)
      (best-greenhouse-pair :min-x min-x :max-x max-x
			    :min-y (1+ column)
			    :max-y max-y :field field)))
    (propose-solution
     (cons-rectangle
      (best-single-greenhouse :min-x min-x :max-x max-x
			      :min-y (1+ column)
			      :max-y max-y :field field)
      (best-greenhouse-pair-horizontal :min-x min-x :max-x max-x
				       :min-y min-y
				       :max-y column :field field)))))
#| possible optimization: first, simplify again the pair region |#

(defun best-greenhouse-triple
  (&key (min-x 0) (max-x *xx*)
	(min-y 0) (max-y *yy*)
	(field *field*))
  (with-solutions
   (propose-sensible-greenhouse-triples
    :min-x min-x :max-x max-x
    :min-y min-y :max-y max-y :field field)))

(defun find-best-greenhouse-triple ()
  (propose-solution (best-greenhouse-triple
		     :min-x 0
		     :max-x *xx*
		     :min-y 0
		     :max-y *yy*)))

(defun same-row (row1 row2)
  (loop for column from ($left *bounding-box*) to ($right *bounding-box*)
    always (eql (aref *field* row1 column) (aref *field* row2 column))))
(defun same-column (column1 column2)
  (loop for row from ($top *bounding-box*) to ($bottom *bounding-box*)
    always (eql (aref *field* row column1) (aref *field* row column2))))
(defun compute-horizontal-stripes ()
  (append (list ($top *bounding-box*))
	  (loop
	    for row from (1+ ($top *bounding-box*))
	    to ($bottom *bounding-box*)
	    unless (same-row row (1- row))
	    collect row)
	  (list (1+ ($bottom *bounding-box*)))))
(defun compute-vertical-stripes ()
  (append (list ($left *bounding-box*))
	  (loop
	    for column from (1+ ($left *bounding-box*))
	    to ($right *bounding-box*)
	    unless (same-column column (1- column))
	    collect column)
	  (list (1+ ($right *bounding-box*)))))
(defun simplify-field ()
  (setf *field*
	(let ((field (make-array (list (1+ *xx*) (1+ *yy*))
				 :initial-element #\.
				 :element-type 'base-char)))
	  (loop for x to *xx* do
	    (loop for y to *yy* do
	      (setf (aref field x y)
		    (aref *field* 
			  (translate-coord-min x *x-row*)
			  (translate-coord-min y *y-column*)))))
	  field)))
(defun simplify-problem ()
  (setf *x-row* (coerce (compute-horizontal-stripes) 'vector)
	*y-column* (coerce (compute-vertical-stripes) 'vector)
	*xx* (- (length *x-row*) 2)
	*yy* (- (length *y-column*) 2)
	*field* (simplify-field)))

(defun merge-arectangles (A B)
  (make-xy-arectangle :top (min ($top A) ($top B))
		      :left (min ($left A) ($left B))
		      :bottom (max ($bottom A) ($bottom B))
		      :right (max ($right A) ($right B))))
(defun rectangles-intersect-p (A B)
  (and (<= (max ($top A) ($top B)) (min ($bottom A) ($bottom B)))
       (<= (max ($left A) ($left B)) (min ($right A) ($right B)))))
(defun point->rectangle (x y)
  (make-rectangle :top x
		  :left y
		  :bottom x
		  :right y))
(defun extend-arectangle/concept (R C)
  (make-xy-arectangle :top (min ($top R) ($x C))
		      :bottom (max ($bottom R) ($x C))
		      :left (min ($left R) ($y C))
		      :right (max ($right R) ($y C))))
(defun rectangle-includes-concept-p (R C)
  (and (<= ($top R) ($x C) ($bottom R))
       (<= ($left R) ($y C) ($right R))))
(defun extend-arectangle (R x y)
  (make-xy-arectangle :top (min ($top R) x)
		      :bottom (max ($bottom R) x)
		      :left (min ($left R) y)
		      :right (max ($right R) y)))
(defun rectangle-includes-p (R x y)
  (and (<= ($top R) x ($bottom R))
       (<= ($left R) y ($right R))))
(defun extend-rectangle (R x y)
  (if R
      (make-rectangle :top (min ($top R) x)
		      :bottom (max ($bottom R) x)
		      :left (min ($left R) y)
		      :right (max ($right R) y))
    (point->rectangle x y)))

(defun whole-field-rectangle ()
  (make-rectangle :top 0 :bottom *xx* :left 0 :right *yy*))
(defun trivial-solution ()
  (list (whole-field-rectangle)))

(defun fill-house (field name rectangle)
  (loop for x from ($top rectangle) to ($bottom rectangle) do
    (loop for y from ($left rectangle) to ($right rectangle) do
      (setf (aref field x y) name))))
(defun make-field-from-rectangles ()
  (loop
    with field = (make-field-from-lines)
    for r in (translate-rectangles *solution*)
    for c across *house-names* do
    (fill-house field c r)
    finally (return field)))
(defun check-and-print-solution ()
  (let ((*field* (make-field-from-rectangles))
	*cost*)
    (check-solution-get-cost)
    (print-solution)
    *cost*))

(defun check-house (name rectangle)
  (loop for x from ($top rectangle) to ($bottom rectangle) do
    (loop for y from ($left rectangle) to ($right rectangle)
    unless (eql name (aref *field* x y))
    do (error (format nil "house ~A not a rectangle" name))))
  (incf *cost* (rectangle->cost rectangle)))
(define-modify-macro merge-rectangles-f (r) merge-rectangles)
(define-modify-macro extend-rectangle-f (x y) extend-rectangle)
(defun check-solution-get-cost ()
  (let ((houses (make-hash-table)))
    (setf *cost* 0)
    (loop for row from 0 below *rows* do
      (loop for column from 0 below *columns*
	for char = (aref *field* row column)
	unless (eql char #\.)
	do (extend-rectangle-f (gethash char houses) row column)))
    (maphash #'check-house houses)
    *cost*))

;;; Elementary concepts are the non-empty rectangles from the simplified array
(defun setup-concepts ()
  (setf *concepts* (loop for x to *xx* nconc
		      (loop for y to *yy*
			unless (empty-cell-p *field* x y)
			collect
			(make-concept :x x :y y)))
	*area* (reduce '+ *concepts* :key '$area)
	*n-concepts* (length *concepts*)))

(defun NIY () (error "NOT IMPLEMENTED YET"))
(defmacro try (&body body)
  `(catch 'fail
     ,@body))
(defun fail ()
  #+TEST (format t "BACKTRACK~%")
  (throw 'fail nil))
(defun estimated-option-cost (option)
  (car option))
(defun enact-option (option)
  (funcall (cadr option)))
(defun sort-options (options)
  (sort options #'< :key 'estimated-option-cost))

;;; Exhaustive search, merging elementary concepts into rectangles
(defmacro try-conceptual (&body body)
  `(try
    (let* ((*rectangles* (copy-seq *rectangles*))
	   (*empty-rectangles* *empty-rectangles*)
	   (*current-cost* *current-cost*)
	   (*n-concepts* *n-concepts*)
	   (*concepts* *concepts*))
      #+TEST (format t "BACKTRACK POINT~%")
      ,@body)))
(defun obvious-fail-check ()
  (when (or (>= *current-cost* *cost*)
	    (> *empty-rectangles* *n-concepts*))
	    (fail)))
(defun propose-solution-rectangles ()
  (setf *solution* (nreverse (coerce *rectangles* 'list))
	*cost* *current-cost*)
  #+TEST (format t "found solution (cost ~A):~% ~A~%"
		 *cost* *solution*))
(defun obvious-completion-check ()
  (when (= *n-concepts* *empty-rectangles*)
    (loop for c in *concepts*
      for i from (1- *empty-rectangles*) downto 0
      do (setf (aref *rectangles* i) c))
    (propose-solution-rectangles)
    (fail)))
(defun new-rectangle-option ()
  (list	(+ *current-cost* (/ (- *cost* *current-cost*) *empty-rectangles*))
	#'(lambda ()
	    (decf *empty-rectangles*)
	    (setf (aref *rectangles* *empty-rectangles*)
		  (car *concepts*))
	    #+TEST (format t "new rectangle ~A: ~A~%" *empty-rectangles*
			   (aref *rectangles* *empty-rectangles*))
	    (setf *concepts* (cdr *concepts*))
	    (decf *n-concepts*))
	#+TEST #+TEST #+TEST  :new (1- *empty-rectangles*) (car *concepts*)))
(defun rectangle-merge-option (i)
  (let* ((concept (first *concepts*))
	 (x ($x concept))
	 (y ($y concept))
	 (rectangle (extend-arectangle (aref *rectangles* i) x y))
	 (cost (+ *current-cost*
		  (- ($area rectangle)
		     ($area (aref *rectangles* i))
		     ($area concept))))
	 (n-concepts (1- *n-concepts*)))
    (when (loop for j from *empty-rectangles* to *max-rectangle*
	    never
	    (and (not (= i j))
		 (rectangles-intersect-p rectangle (aref *rectangles* j))))
      (let ((concepts
	     (loop for c in (rest *concepts*)
	       if (rectangle-includes-p rectangle ($x c) ($y c))
	       do (progn (decf cost ($area c)) (decf n-concepts))
	       else collect c)))
	(when (and (< cost *cost*) (>= n-concepts *empty-rectangles*))
	  (list cost
		#'(lambda ()
		    #+TEST (format t "extend rectangle ~A: ~A~%"
				   i rectangle)
		    (setf *current-cost* cost
			  *concepts* concepts
			  *n-concepts* n-concepts
			  (aref *rectangles* i) rectangle))
		#+TEST #+TEST #+TEST :merge i (car *concepts*)))))))
(defun find-possible-options ()
  (let ((merging-options
	 (loop for rectangle from *max-rectangle* downto *empty-rectangles*
	   for option = (rectangle-merge-option rectangle)
	   when option collect option)))
    (if (< 0 *empty-rectangles*)
	(cons (new-rectangle-option) merging-options)
      merging-options)))
(defun complete-rectangles ()
  #+TEST (print-current-state)
  (obvious-completion-check)
  (let ((options (find-possible-options)))
    (try-options-conceptual options)))
(defun init-concept-search (n)
  (setf *n* n
	*search-strategy* 'conceptual
	*current-cost* (+ (* 10 n) *area*)
	*rectangles* (make-array (list n) :initial-element nil)
	*max-rectangle* (1- n)
	*empty-rectangles* n))
(defun print-current-state ()
  (format t "  cost ~A / ~A~%" *current-cost* *cost*)
  (let ((field (make-field-from-lines)))
    (loop for i from *max-rectangle* downto *empty-rectangles*
      for c across *house-names*
      do (fill-house field c (translate-rectangle (aref *rectangles* i))))
    (loop for r in *concepts*
      for c across *concept-names*
      do (fill-house field c (translate-rectangle r)))
    (print-field t field)))
(defun commit-option-conceptual (option)
  (enact-option option)
  #+TEST (format t " cost ~A: remaining concepts (~A):~%  ~A~%"
		 *current-cost* *n-concepts* *concepts*)
  (complete-rectangles))
(defun try-options-conceptual (options)
  (cond
   ((null options) (fail))
   ((null (rest options)) (commit-option-conceptual (first options)))
   (t (loop for option in (sort-options options) do
	(try-conceptual (commit-option-conceptual option))))))
(defun find-best-greenhouses-conceptual (n)
  (init-concept-search n)
  (try-conceptual
   (obvious-fail-check)
   (complete-rectangles)))

;;; Heuristic search: consider the most interesting concept at each step
(defvar *challenge-concept*)
(defvar *challenge-interest*)
(defvar *challenge-options*)
(defvar *challenge-noptions*)
#+DEBUG
(progn
  (defvar *competing-concepts*)
(defun show-competing-concepts ()
  (sort *competing-concepts* #'< :key 'second)
  (format t "  min interest: ~A~%"
	  (coerce (cadar *competing-concepts*) 'float))
  (let ((field (make-field-from-lines)))
    (loop for r across *rectangles*
      for c across *house-names*
      do (fill-house field c (translate-rectangle r)))
    (loop for (r i o) in *competing-concepts*
      for c across *concept-names*
      do (fill-house field c (translate-rectangle r)))
    (print-field t field)))
)
(defun challenge-concept (concept interest noptions options)
  #+DEBUG (push (list concept interest options) *competing-concepts*)
  (when (or (zerop noptions)
	    (and (= 1 noptions) (= *n* *n-rectangles*)))
    (throw 'forced-choice (values concept interest noptions options)))
  (when (or (not *challenge-concept*)
	    (> interest *challenge-interest*)
	    (and (= *challenge-interest*)
		 (< noptions *challenge-noptions*)))
    (setf *challenge-interest* interest
	  *challenge-noptions* noptions
	  *challenge-options* options
	  *challenge-concept* concept)))
(defmacro with-concept-challenge (&body body)
  `(let (*challenge-concept* *challenge-interest*
         #+DEBUG *competing-concepts*
	 *challenge-noptions* *challenge-options*)
     (catch 'forced-choice
       ,@body
       #+DEBUG (show-competing-concepts)
       (values *challenge-concept* *challenge-interest*
	       *challenge-noptions* *challenge-options*))))
(defvar *options* ())
(defmacro try-heuristic (&body body)
  `(try
    (let* ((*rectangles* *rectangles*)
	   (*n-rectangles* *n-rectangles*)
	   (*current-cost* *current-cost*)
	   #+TEST (*options* *options*)
	   (*n-concepts* *n-concepts*) ;;; NB: as opposed to -conceptual, holds number of concepts *including* rectangles
	   (*concepts* *concepts*))
	 #+TEST (format t "BACKTRACK POINT~%")
	 ,@body)))
(defun obvious-heuristic-fail-check ()
  (when (>= *current-cost* *cost*) ; or (< *n-concept* *n*)
    (fail)))
(defun propose-solution-rectangles-heuristic ()
  (setf *solution* (coerce *rectangles* 'list)
	*cost* *current-cost*)
  #+TEST (format t "found solution (cost ~A):~% ~A~%"
		 *cost* *solution*)
  nil)
(defun obvious-heuristic-completion-check ()
  (when (= *n-concepts* *n*)
    (heuristic-complete-and-propose-solution)
    (fail)))
(defun new-rectangle-heuristic-option (c)
  ;(when (< *current-cost* *cost*) ...)
  (list 0
	#'(lambda ()
	    (setf *rectangles* (concatenate 'vector *rectangles* (list c))
		  *concepts* (remove c *concepts*))
	    #+TEST (format t "new rectangle ~A: ~A~%"
			   *n-rectangles* c)
	    (incf *n-rectangles*)
	    (decf *remaining-area* ($area c)))
	#+TEST #+TEST #+TEST :new *n-rectangles* c))
(defun merge-rectangle-heuristic-option (cost interest remaining-area
					      c i mr n-concepts concepts)
  (declare (ignorable c))
  (when (< cost *cost*)
    (list interest
	  #'(lambda ()
	      (setf *rectangles* (copy-seq *rectangles*)
		    (aref *rectangles* i) mr
		    *concepts* concepts
		    *n-concepts* n-concepts
		    *remaining-area* remaining-area
		    *current-cost* cost)
	      #+TEST (format t "merged rectangle ~A with ~A: ~A~%"
			     i c mr)
	      #+TEST #+TEST #+TEST :merge i c))))
(defun consider-terminal-heuristic-option (cost c i mr concepts)
  (declare (ignorable c))
  (let ((*rectangles* (concatenate 'list *rectangles* concepts))
	(*current-cost* cost))
    (setf (nth i *rectangles*) mr)
    (propose-solution-rectangles-heuristic)
    nil))
(defun heuristic-complete-and-propose-solution ()
  (setf *rectangles* (concatenate 'list *rectangles* *concepts*))
  (propose-solution-rectangles-heuristic))
(defun consider-merge (c r i)
  (let* ((mr (extend-arectangle/concept r c)))
    (when (loop for o across *rectangles*
	    never (and (not (eql r o)) (rectangles-intersect-p mr o)))
      (let* ((coverage (- ($area mr) ($area r)))
	     (covered-area 0)
	     (n-concepts *n-concepts*)
	     (concepts (loop for x in *concepts*
			 if (rectangle-includes-concept-p mr x)
			 do (progn (incf covered-area ($area x))
				   (decf n-concepts))
			 else collect x))
	     (lossage (- coverage covered-area))
	     (cost (+ *current-cost* lossage)))
	(when (and (< cost *cost*) (>= n-concepts *n*))
	  (let* ((remaining-area (- *remaining-area* covered-area))
#| needlessly complex:
		 (interest (/ (* lossage remaining-area)
			      (* (- *cost* cost) coverage)))
  too simple:
		 (interest lossage))
 |#
		 (interest (+ lossage (- *n-concepts* n-concepts))))
	    (merge-rectangle-heuristic-option cost interest remaining-area
					      c i mr
					      n-concepts concepts)))))))
(defun consider-concept-options (c)
  (loop
    with options = ()
    with noptions = 0
    with interest = *max-interest*
    for r across *rectangles*
    for i from 0
    for option = (consider-merge c r i)
    do (when option
	 (push option options)
	 (incf noptions)
	 (setf interest (min interest (car option))))
    finally (return (values interest noptions options))))
(defun most-discriminating-concept ()
  (with-concept-challenge
   (dolist (c *concepts*)
     (multiple-value-bind (interest noptions options)
	 (consider-concept-options c)
       (challenge-concept c (- interest noptions)
			  noptions options)))))
#| Use one of these:
interest
(- interest noptions)
(- (* 10 interest) noptions)) or
(/ interest noptions)
|#
(defun problem-split-options-heuristic ()
  (multiple-value-bind (concept interest noptions options)
      (most-discriminating-concept)
    (declare (ignore interest noptions))
    (when (< *n-rectangles* *n*)
      (let ((option (new-rectangle-heuristic-option concept)))
	(when option
	  (push option options))))
    options))
(defun commit-option-heuristic (option)
  (enact-option option)
  #+TEST (format t " cost ~A: remaining concepts:~%  ~A~%"
		 *current-cost* *concepts*)
  (complete-rectangles-heuristic))
(defun try-options-heuristic (options)
#+TEST (setf *options* options)
  (cond
   ((null options) (fail))
   ((null (rest options)) (commit-option-heuristic (first options)))
   (t (loop for option in (sort-options options) do
	(try-heuristic (commit-option-heuristic option))))))
(defun complete-rectangles-heuristic ()
  #+TEST (print-current-heuristic-state)
  (obvious-heuristic-fail-check)
  (obvious-heuristic-completion-check)
  (let* ((options (problem-split-options-heuristic)))
    (try-options-heuristic options)))
(defun init-heuristic-search (n)
  (setf *n* n
	*search-strategy* 'heuristic
	*rectangles* #()
	*n-rectangles* 0
	*cost* (or *cost* (+ 11 (* *rows* *columns*)))
	*remaining-area* *area*
	*current-cost* (+ (* 10 n) *area*)))
(defun print-current-heuristic-state ()
  (format t "  cost ~A / ~A~%" *current-cost* *cost*)
  (let ((field (make-field-from-lines)))
    (loop for r across *rectangles*
      for c across *house-names*
      do (fill-house field c (translate-rectangle r)))
    (loop for r in *concepts*
      for c across *concept-names*
      do (fill-house field c (translate-rectangle r)))
    (print-field t field)))
(defun find-best-greenhouses-heuristic (n)
  (try-heuristic
   (init-heuristic-search n)
   (complete-rectangles-heuristic)))

(defun s1 () (setf *options* (problem-split-options-heuristic)))
(defun s2 (&optional (i 0)) (enact-option (nth i *options*)))

(defun test1 ()
  (with-open-file (i (concatenate 'string *ita-dir* "rectangles-1.txt")
		     :direction :input)
    (read-problem i)
    (setf *cost* (check-solution-get-cost))
    (unless (= *cost* *max-greenhouses*)
      (error (format nil "bad reported cost ~A but really was ~A"
		     *max-greenhouses* *cost*)))
    (print-field)))

(defun test2 (&optional (n 1))
  (with-open-file (i (concatenate 'string *ita-dir* "rectangles.txt")
		     :direction :input)
    (loop repeat n do
      (read-problem i))
    (time (solve-problem))))

(defun test3 ()
  (format t "~A~%" *version*)
  (time
   (let* ((costs
	   (with-open-file (i (concatenate 'string *ita-dir* "rectangles.txt")
		     :direction :input)
	     (loop repeat 8 do
	       (read-problem i)
	       collect (time (solve-problem)))))
	  (total-cost (reduce '+ costs)))
     (format t "~%Total cost: ~A  ~A~%" total-cost costs))))

(defun test4 ()
  (with-open-file (i (concatenate 'string *ita-dir* "r.txt")
		     :direction :input)
   (let* ((costs
	   (loop repeat 9 do
	     (read i)
	     (read-field i)
	     (check-solution-get-cost)
	     (print-solution)
	     (format t "~%")
	     collect *cost*))
	  (total-cost (reduce '+ costs)))
     (format t "~%Total cost: ~A  ~A~%" total-cost costs))))

(defun test5 (&optional (n 1))
  (with-open-file (i (concatenate 'string *ita-dir* "rectangles.txt")
		     :direction :input)
    (loop repeat n do
      (read-problem i))
    (time (solve-problem-heuristic))))

(defun test6 ()
  (format t "~A~%" *version*)
  (time
   (let* ((costs
	   (with-open-file (i (concatenate 'string *ita-dir* "rectangles.txt")
		     :direction :input)
	     (loop repeat 8 do
	       (read-problem i)
	       collect (time (solve-problem-heuristic)))))
	  (total-cost (reduce '+ costs :initial-value 359)))
     (format t "~%Total cost: ~A  ~A~%" total-cost costs))))

(defun find-mostest (list &optional (compare #'<)
			  &key (key #'identity))
  (when list
    (loop with best = (first list)
      with score = (funcall key best)
      for candidate in (rest list)
      for challenge = (funcall key candidate)
      when (funcall compare challenge score)
      do (setf best candidate score challenge)
      finally (return best))))
(defun copy-array-shape (array)
  "make a new array of same shape as given array"
  (make-array (array-dimensions array)
	      :element-type (array-element-type array)))
(defun copy-array (array)
  "make a fresh (shallow) copy of an array"
  (let ((new-array (copy-array-shape array)))
    (loop for i below (array-total-size array) do
      (setf (row-major-aref new-array i) (row-major-aref array i)))
    new-array))

(defun setup-problem ()
  (setf *field* (make-field-from-lines)
	*xx* (1- *rows*)
	*yy* (1- *columns*)
	*x-row* (make-array (list (1+ *rows*))
			    :initial-contents (iota 0 *rows*))
	*y-column* (make-array (list (1+ *columns*))
			       :initial-contents (iota 0 *columns*))
	*bounding-box* (best-single-greenhouse)
	*solution* nil
	*cost* nil))

(defvar *cap* 10)
(defun too-hard (n) (and (> n *cap*) (> (length *concepts*) 64)))
(defun solve-problem ()
  (setup-problem)
  (when *bounding-box*
    (simplify-problem)
    (setf *solution*
	  (with-solutions
	   (find-best-single-greenhouse)
	   (when (<= 2 *max-greenhouses*)
	     (find-best-greenhouse-pair))
;	   (when (<= 3 *max-greenhouses*)
;	     (find-best-greenhouse-triple))
	   (when (<= 3 *max-greenhouses*)
	     (setup-concepts)
	     (loop for n from 2 to *max-greenhouses*
	       unless (too-hard n)
	       do (find-best-greenhouses-conceptual n))))))
  (check-and-print-solution))
(defun solve-problem-heuristic ()
  (setup-problem)
  (when *bounding-box*
    (simplify-problem)
    (setup-concepts)
    (setf *solution*
	  (with-solutions
	   (find-best-single-greenhouse)
	   (loop for n from 2 to *max-greenhouses*
	     unless (too-hard n)
	     do (find-best-greenhouses-heuristic n)))))
  (check-and-print-solution))
(defun filter ()
  (loop while (peek-char nil *standard-input* nil) do
    (read-problem *standard-input*)
    (solve-problem-heuristic)
    (format t "~%")))

(defun opt ()
  (proclaim '(optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (delete :debug *features*)
  (compile-file (concatenate 'string *lisp-dir* "ita-strawberries.lisp"))
  (load (concatenate 'string *lisp-dir* "ita-strawberries")))
(defun dbg ()
  (proclaim '(optimize (speed 3) (space 0) (safety 3) (debug 3)))
  (pushnew :debug *features*)
  (compile-file (concatenate 'string *lisp-dir* "ita-strawberries.lisp"))
  (load (concatenate 'string *lisp-dir* "ita-strawberries")))

(defun pc ()
  (format t "~A/~A " (- *n* *empty-rectangles*) *n*)
  (print-current-state))
(defun ph ()
  (format t "~A/~A " *n-rectangles* *n*)
  (print-current-heuristic-state))
(defun ps () (format t "~A / " *cost*) (check-and-print-solution))
(defun pp () (ps) (if (eq *search-strategy* 'conceptual) (pc) (ph)))

#|
A few notes.

Actually, this programs implements several solutions of increasing complexity
and increasing performance. Only the last one is an exact solution that
solves all the examples in reasonably finite time. It is the one we describe.

Our simplification step.
* We group together identical rows and also identical columns.
* Indeed, for any solution that splits two identical rows (resp. columns),
 there exists a strictly better solution that doesn't.
* Proof: let's consider as an auxiliary cost function for all considered
 coverages the number of pairs of neighbouring locations on identical rows
 that are being distinguished. If the auxiliary cost of a coverage is not zero,
 let's consider the topmost leftmost such location. Left to it is a blank.
 Let's consider rectangles rightwards across this separation from there to
 the next point where the rows are no more distinguished by rectangles
 (blank, border of the field or rectangle).
 We can rearrange the rectangles above (resp. below) the separation so that
 there be only one along the separation line, whose height is that of the
 smallest of considered rectangles. After doing that both above and below,
 we can merge the two rectangles at the separation line. QED.
* Note that this proof actually works for rows and columns
 that are distinguished (after dropping rectangle names) *in the solution*.
 This might allow for dynamic simplication if shapes were merged that
 help distinguish less rows or columns, and/or for "forced choice"
 of a way to discriminate two consecutive rectangles.
* Also note that the proof works on "locally identical" rows and columns
 of the solution field, up to next blanks alongside the given direction.
 However, because of mergers, we can't be sure which they will be based
 on the locally identical rows and columns of the initial field,
 unless we're sure there will be no lossage.
* Now, using the same "reduction", considering a maximal rectangle in
 the initial problem (or an intermediate problem or solution), such as e.g.
 the mouth in the last example, if said maximal rectangle is split by an
 optimal solution, then by the previous theorem, the splitting rectangles
 must go at least in two different directions, and/or there must be no
 rectangle left that doesn't reach far away.
 As @.....@...... with *n*=2 AAAAAAA......
 in ......@.....@  yielding  ......BBBBBBB
* These could help tremendously when some "easy" mergers or non-mergers
 are suggested by a search strategy that would split the problem
 into disjoint components early on based on an analysis of the lossage
 involved in joining them.

General strategy concerning the number of rectangles:
* Do a search for a given number of rectangles. Start with one, and go up:
 Solutions for lower numbers of rectangles are easier to compute,
 may provide the actual solution, and serve as a cost cap that helps
 restrict the search space for higher numbers.
* Doing a search without a fixed number of rectangles takes a lot more time,
 due to the absence of this cheap cap.

Our "conceptual" search:
* a concept is the seed of a greenhouse: a rectangle in the field.
 A committed concept (also called rectangle) that has been picked as
 Non-committed concepts are 1x1 (in the simplified field).
* next concept to consider is the topmost rightmost strawberry
 among the remaining non-committed ones.
* branch to commit it to one of a new rectangle or an existing rectangle
* cut branches that already cost more than best solution
 or don't have enough rectangles or concepts to 
* when we run out of concepts and yet didn't equal or exceed cost,
 we have found a better solution.
* Result: this method works pretty fast on all the examples but the last.
 After several days of computation on the last example on a 3GHz machine,
 this search was still wasting its time exploring the search space for n=9,
 though it had found a solution of cost 369 (best for n=9).

Our "heuristic" search:
* About the same as above, but the "next" concept we pick
 is the most discriminating one, according to some heuristic.
* The heuristic tries to evaluate the prospect of picking this concept:
 how nearer to completing the search (in a positive or negative way)
 does it make us? How much compulsory wastage does it reveal?
 We basically try to evaluate how many bits of information we get
 for a given computational cost, so as to maximize the bang per buck;
 however, we don't have an exact evaluation of the interest of a branch
 (if we did, we'd already have solved the problem),
 so we pick a rough estimate (and that's why the method is called "heuristic",
 as opposed to "algorithmic").
* if there is only one option of rectangle for concept, it's a forced choice,
 so make it before to continue (if there is no option left, cut the branch).
* otherwise, our heuristic is to consider as the interest of an option
 the "lossage" due to picking the rectangle
 (area covered that didn't have strawberries on it),
 to which we may add the number of concepts removed);
 the idea is that the greater this "interest",
 the shorter the search depth of underlying branch before we may cut.
 The interest of a concept is the least interest of an option of it,
 since with the exponentional explosion in cost due to recursive branching,
 the cost of exploring a the longer sub-branch will dominate the cost
 of exploring all shorter branches. We may amend the interest of a concept
 by substracting the number of options from it, since the number of options
 nevertheless impact negatively the cost of branching at this concept.
 The concept with highest interest is thus that with least expected cost.
* The heuristic doesn't have to be perfect; it has to be good enough to
 significantly reduce the effective depth of the search space, yet fast enough
 so that it makes sense to take time to evaluate on which concept to branch
 instead of blindly picking the first one as before. A "better" heuristic
 must improve on the gains without costing too much.

Potential improvements to this line of search strategies:
* Tweak the heuristic function to find better choices.
 This means having a better model to evaluate the depth and cost
 of the method as applied to some intermediate problem.
* Cache more things along branches of heuristic search?
 e.g. maybe it makes sense to remember which existing rectangles a concept
 can (still) be possibly merged into. Can the number of such extension
 opportunities be used as part as computing the interest of a merge?
* Switch from "heuristic" to "conceptual" when so few concepts are left
 that it doesn't make sense picking the best one,
 especially after there remains no new rectangles to create.
* Assuming the solution with an increased n is "similar" enough
 to the solution with previous values of n, find the initial concepts
 of a search from previous best solution with lesser values of *n*.
 Thus we may arrive faster at the solution, and sooner find a cap to the cost
 that allows to the remaining branches faster.
* Find better ways to evaluate the "necessary" lossage associated
 to the remaining concepts.
 We could then not only improve the concept-picking heuristic,
 but more importantly, speed up the cutting of rotten branches.
* Allocate things without consing, with either dynamic-extent declaration,
 or a manually-managed stack. That's low-level hacking that may provide
 a significant (3-fold?) constant speed up factor.
* Maybe produce a generic code-walker and/or domain specific language
 to automatically produce optimized C code that would compute the answer;
 thus we get the benefits of the previous approach semi-automatically,
 with a reusable tool. Instead of reinventing the wheel,
 maybe it's just a matter of using an existing Lisp->C compiler like ThinLisp.
* Parallelize the search. Maintain an explicit queue of the branching options
 waiting to be examined, and whenever there's a free processor,
 have it "steal" and process the pending option nearest to the trunk.
 Don't forget to broadcast the new challenging cost
 whenever a better solution has been found.
 There again, this parallelization could be semi-automated
 by developing an adequate domain-specific language.

Research tracks for higher-level heuristics:
* When there are many concepts, the key seems to be in devising ways
 to represent the tradeoffs made necessary to integrate this concept
 into a solution, considering the relatively low number of rectangles
 with respect to concepts.
 Q: are there better ways?
* Split the problem early on into disjoint components based on
 an analysis of the lossage involved in joining them.
 Q: how to conduct such an analysis?
* When making a "long" rectangle, we can make it thicker at great cost
 (or maybe not?), and it also splits the field in two, increasing the need
 for rectangles on both sides (instead of having transversal rectangles).
 Q: Can we quantify the lossage due to such rectangles in such a way as to
 easily rule them out and then establish disjoint components?
* When we have several components, compute the "potential" in extending
 the number of rectangles per component.
* Look for symmetries in the considered components to simplify the search?
 Q: how do we simplify the search based on a symmetry?
* Take a more "global" approach to rectangle search,
 picking whole rectangles at once? Start by generating rectangles
 that are not too "lossy", according to the above analysis?
* Note that each rectangle is a k-block as determined
 by k extreme elements with k<=4. That's at most n^4 of them,
 actually much fewer. Generating k+1-block from k-blocks.
 With only 10 blocks max, we have fewer than n^40 combinations.
 yay, there is a deterministic polynomial algorithmic solution!
 Q: can we proactively eliminate those that are much too lossy?
* Q: how much can we reuse from the analysis with lower values of n?
* Q: Compute "proximity" graph of concepts modulo not-too-lossy k-blocks?
* Q: can we do "proximity" reasoning, as in "if we choose this rectangle
 for this concept, then this restricts the potential not-too-lossy rectangles
 for its neighbours"?
* Q: can we get better lower bound for the current cost, based on knowing
 that "on the average" a rectangle will have to cover so many concepts, or
 otherwise based on some variant of the pigeon-hole principle?
* Q: can we get a better strategy (better choice points and/or better
 constraints) by forcing decisions of the kind "the solution will or won't
 separate these two concepts" without having a list of committed disjoint
 concepts? How would we choose the pairs to either merge or include?

Note: it took about sixteen hours of cpu time to solve the ninth example
on my Duron 1200 with CMUCL 18e.
Almost 9 hours on a AMD64 2.4GHz with SBCL 0.9.0.19.

Silly benchmark: this program compiled by Genera on my MacIvory3 LispM is
one to two times faster than interpreted by GNU clisp on my Jornada 820 and
about one hundred times slower than compiled by CMUCL 18e on my Duron 1200.
Of course, considering the amount of (mostly superfluous) consing, between
the two compiled platforms, I may very well be testing mostly but relative
RAM speed.


Below is a shell script to use this program in Unix batch mode with CMUCL:

END-OF-LISP
Solving a simple problem from ITA

#!/bin/sh

### This script launches the filter function, which repeatedly expects
### rectangles as input, and gives proven optimal solutions as output

## You may specify your LISP implementation in environment variable LISP,
## or by editing this file to define yours (if not yet defined) and/or
## give it higher priority.

DIR=$(dirname $0)
PROG=ita-strawberries

### Try to find a lisp among the known working ones
set_cmucl () {
  LISP="cmucl"
  OPTIONS="-quiet -batch -noinit"
  LOAD=-load
  EVAL=-eval
  FASL=x86f
}
set_sbcl () {
  LISP="sbcl"
  OPTIONS="--noinform --sysinit /dev/null --userinit /dev/null"
  LOAD=--load
  EVAL=--eval
  FASL=fasl
}
set_lisp () {
  set_cmucl
}
set_clisp () {
  LISP="clisp"
  OPTIONS="-norc"
  LOAD=-i
  EVAL=-x
  FASL=fas
}

trylisp () {
  if LISPBIN=`which $1` ; then set_$1 ; return 0 ; else return 1 ; fi
}
{ [ -n "$LISP" ] && trylisp $LISP ; } ||
trylisp sbcl ||
trylisp cmucl ||
trylisp lisp ||
trylisp clisp ||
{ echo "Cannot find lisp implementation." ; exit 42 ;
}

trydir () {
  if [ -f $1/$PROG.lisp ] ; then L=$1 ; return 0 ; else return 1 ; fi
}

trydir ./ ||
trydir $DIR ||
trydir /local/fare/fare/lisp/puzzle/ ||
trydir /home/fare/fare/lisp/puzzle/ ||
trydir /mnt/fare/lisp/puzzle/ ||
{ echo "Cannot find lisp code." ; exit 69 ; }

## Compile the first time around, or if the source has changed.
## Thus, next times, we can load directly and have correct timing.
## Compiling separately also allows to ditch those nasty compilation warnings
## to the bit bucket they deserve. Me, write errors in Lisp code? Never!
if [ ! -f "$L/$PROG.$FASL" -o "$L/$PROG.$FASL" -ot "$L/$PROG.lisp" ] ; then
  $LISP $OPTIONS $EVAL "(progn (compile-file \"$L/$PROG.lisp\") (quit))" \
  < /dev/null > /dev/null 2>&1
fi


## Now at last, invoke lisp with our compiled program...
$LISP $OPTIONS $EVAL "(progn (load \"$L/$PROG.$FASL\") (filter) (quit))"

#\/ |#
