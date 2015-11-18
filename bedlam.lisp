;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

;; run me as one of:
;;	cl -s fare-puzzles/bedlam

;;; Solver for the Bedlam cube puzzle

;; Bedlam cube: fit the following pieces in a 4x4x4 3D cube
;; pieces they can be rotated, but not symmetrized.
;; Problem originally submitted to me by Rik Rose.
;; http://rikrose.net/soma/
;; http://ch.twi.tudelft.nl/~sidney/puzzles/
;; http://www.bedlamcube.com/

; begun Fri Aug 31 15:23:00 CEST 2001
; ended ... Sep 27 ...

;;; Implementation notes: see on the end of file

;;; Do things in their own package, but allow reloading.
(uiop:define-package :fare-puzzles/bedlam
  (:mix :fare-utils :uiop :alexandria :common-lisp)
  (:export :solve-problem))
(in-package :fare-puzzles/bedlam)

;;; Description of the pieces
;;; as described by existance in a 3d grid (x,y,z)
(defparameter =piece-list= '(
((0 0 0) (0 0 1) (0 0 2) (1 0 0) (1 1 0))
((0 0 0) (1 0 0) (0 1 0) (0 2 0) (0 2 1))
((0 0 0) (0 1 0) (1 1 0) (0 1 1) (0 2 0))
((0 0 0) (0 0 1) (0 1 0) (1 1 0))	  ; RUNT piece, only 4 little cubes
((0 0 0) (1 0 0) (1 1 0) (2 1 0) (2 2 0))
((0 0 0) (1 0 0) (2 0 0) (2 1 0) (2 0 1))
((0 0 0) (0 1 0) (1 1 0) (0 1 1) (0 2 1))
((0 0 0) (0 0 1) (1 0 0) (1 1 0) (2 1 0))
((0 1 0) (1 0 0) (1 1 0) (1 2 0) (2 1 0)) ; flat cross of 5 little cubes.
((0 0 0) (1 0 0) (0 1 0) (0 1 1) (0 2 0))
((0 0 0) (0 1 0) (0 2 0) (1 1 0) (1 1 1))
((0 0 0) (0 1 0) (0 1 1) (1 1 1) (1 2 1))
((0 0 0) (1 0 0) (1 1 0) (1 2 0) (2 1 0)))
"list of pieces in the puzzle, as a list of 0-based x,y,z coordinates")

;;;;;;;;;;;;;;;;;;;;;;;;;; below is my would-be solver ;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (speed 3) (space 0) (safety 0) #-genera (debug 0)))
#+harlequin-common-lisp
(declaim (optimize (harlequin-common-lisp:fixnum-safety 3)))

;; debugging settings
;(pushnew :do-test *features*) ; enable tests
;(pushnew :do-debug *features*) ; enable heavy debugging

;Choose one and only one of those:
;(pushnew :bitmask *features*) ; use bitmasks for shapes
;(pushnew :xbitmask *features*) ; use expanded bitmasks for shapes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (or #+(or cmu sbcl) t)
    (pushnew :declare-types *features*))
  (unless (or #+(or bitmask xbitmask) t)
    (pushnew 
     (if (or #+(or cmu sbcl) t)
	 :xbitmask
       :bitmask)
     *features*)))

#+CMU (setf extensions:*gc-verbose* nil)
#+CLISP (setf custom:*warn-on-floating-point-contagion* nil)


;;; Define a few constants for the problem.
(defparameter *search-tree-id* "bedlam-11"
  "object identifying the search tree, for restarting searches")

(defparameter =cube-dimensions= '(4 4 4)
  "dimensions of the big cube")
(defparameter =cubes= (apply #'* =cube-dimensions=)
  "number of little cubes")
(defparameter =dimensions= (length =cube-dimensions=)
  "dimensions of the big cube")
(defparameter =max-coordinate= (apply #'max =cube-dimensions=)
  "maximum value for a coordinate")
(defconstant =pieces= 13
  "number of pieces")
(defconstant =max-pindex= 12
  "maximum index for pieces")

; sanity check
(unless (= =dimensions= 3)
  (error "Program written for a 3-dimension problem
and submitted with a different dimensionality."))

;;; Define a few types for the problem.
;;; They can be used either when making sanity checks
;;; or when optimizing the solver for speed.
(deftype piece ()
  "piece identifier"
  `(integer 1 13))
(deftype maybe-piece ()
  "either a piece or nothing (0)"
  '(integer 0 13))
(deftype pindex ()
  "piece index"
  '(integer 0 12))
(deftype maybe-pindex ()
  "piece index"
  '(integer -1 12))
(deftype cube-array ()
  "array with positions of pieces"
  '(simple-array t (4 4 4)))
(deftype cindex ()
  "cube index"
  '(integer 0 63))
(deftype coordinate ()
  "space coordinate, with margin for overflow"
  'fixnum)
(deftype point ()
  "3D point a list of coordinates"
  #+declare-types
   '(cons coordinate
    (cons coordinate
    (cons coordinate null)))
  #-declare-types
   'list)
(deftype simple-shape ()
  "list of 4 or 5 points"
  #+declare-types
   '(cons point (cons point (cons point (cons point
    (or (cons point null) null)))))
  #-declare-types
   'list)
(deftype cil-shape ()
  "list of 4 or 5 cindex"
  #+declare-types
   '(cons cindex (cons cindex (cons cindex (cons cindex
     (or (cons cindex null) null)))))
  #-declare-types
   'list)
(deftype bitmask-shape ()
  "bitmask of cubes used"
  '(unsigned-byte 64))
(deftype ub32 ()
  "32-bit word"
  '(unsigned-byte 32))
(deftype xbitmask-shape ()
  "two 32-bit words, little endian"
  '(simple-array ub32 (2)))
(defsubst make-xbitmask-shape (lo hi)
  (the* xbitmask-shape
    (make-array '(2)
		:element-type 'ub32
		:initial-contents (list (the* ub32 lo) (the* ub32 hi)))))
(defmacro xbitmask-shape-lo (xs)
  `(aref (the* xbitmask-shape ,xs) 0))
(defmacro xbitmask-shape-hi (xs)
  `(aref (the* xbitmask-shape ,xs) 1))
(deftype encoded-shape ()
  "encoded shape"
  #+cil 'cil-shape
  #+bitmask 'bitmask-shape
  #+xbitmask '(vector ub32))
(defstruct (piece-shapes (:type list))
  "a piece, and list of spotted-shapes for that piece"
  (pindex -1 :type maybe-pindex)
  (spotted-shapes-vector (make-array '(64) :element-type 'list
				     :initial-element nil)
			 :type (vector list 64)))
(defmacro piece-shapes-spotted-shapes (piece spot)
  `(aref (the* (vector list 64) (piece-shapes-spotted-shapes-vector ,piece))
	 ,spot))

;;; Compute 3D rotations

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkrot-form (newcoords)
    `#'(lambda (point)
       #+declare-types (declare (type point point))
       (apply #'(lambda (x y z)
		  #+declare-types (declare (type coordinate x y z))
		  (list ,@newcoords)) point))))
(defmacro mkrot (newcoords)
  (mkrot-form newcoords))

(defparameter =rotations=
  ; Generating all rotations by hand is disgraceful, and I'm too lazy for it.
  ; Let's have fun generating rotations at macro-expansion time instead.
  ; for more on rotations of the cube, see
  ; http://home8.inet.tele.dk/bundgard/SOMA/NEWS/N991201.HTM
  ; http://www.bib.ulb.ac.be/coursmath/polyedre.htm
  ; http://www.bib.ulb.ac.be/coursmath/snub.htm
  (macrolet
    ((generate-rotations ()
      (#+DO-TEST time #-DO-TEST progn
       (labels
	   ; find all rotations whose coordinates are
	   ; either S or (- =max-coordinate= S)
	   ; where S is one of the coordinate X Y or Z.
	   ((opp (a)  ; opposite coordinate
	      (if (listp a) (third a) `(- =max-coordinate= ,a)))
	    (symmetric (p) ; opposite point
	      (mapcar #'opp p)))
       (declare (optimize (speed 3) (safety 0) (debug 0)))
       (let*
	   ((direct-permutations '((x y z) (y z x) (z x y)))
	    (indirect-permutations '((x z y) (y x z) (z y x)))
	    (direct-anti-permutations
	     (mapcar #'symmetric indirect-permutations))
	    (signed-permutations
	     (append direct-permutations direct-anti-permutations))
	    (sign-changes
	     (list
	      #'(lambda (x y z) (list x y z))
	      #'(lambda (x y z) (list x (opp y) (opp z)))
	      #'(lambda (x y z) (list (opp x) y (opp z)))
	      #'(lambda (x y z) (list (opp x) (opp y) z))))
	    (rotations
	     (mapcan #'(lambda (s)
			 #+declare-types (declare (type function s))
			 (mapcar #'(lambda (list) (apply s list))
				 signed-permutations))
		     sign-changes)))
	   `(list ,@(mapcar #'mkrot-form rotations)))))))
    (generate-rotations))
  "list of valid rotations for the 3D cube,
as functions using lists of coordinates")

;;; Define the variables that encode the search space.
(defun empty-cube ()
  "state of cube (which little cube filled with which piece)."
  (make-array =cube-dimensions=
    :element-type t
    :initial-element 0))

(defparameter *cube*
  (empty-cube)
  "state of cube (which little cube filled with which piece).")
(declaim (type cube-array *cube*))

(defparameter =cube-unflattener=
  (make-array =cubes=
    :element-type 'point
    :initial-contents
      (loop for i below (first =cube-dimensions=) nconc
        (loop for j below (second =cube-dimensions=) nconc
          (loop for k below (third =cube-dimensions=) collect (list i j k)))))
  "maps flattened cube back to cube.")

(defparameter translated-rotated-shapes '()
  ; see function initialize-translated-rotated-shapes below
  "vector of all the rotated and translated shapes of each piece")
(declaim (type list translated-rotated-shapes))

(defparameter initial-search-shapes '()
  ; see function initialize-initial-search-shapes below
  "same as above, ordered according to the preferred search order")
(declaim (type list initial-search-shapes))

(defparameter *search-shapes* '()
  ; see function initialize-initial-search-shapes below
  "current list of list of shapes to search for remaining pieces")
(declaim (type list *search-shapes*))

(defsubst null-encoded-shape ()
  "null shape to put in partial solution path"
  #+cil '()
  #+bitmask 0
  #+xbitmask (make-xbitmask-shape 0 0))

;;; Variables used to store solutions.
(defparameter *solutions* '()
  "list of solutions found")
(declaim (type list *solutions*))
(defparameter *solutions-count* 0
  "list of solutions found")
(declaim (type fixnum *solutions-count*))
(defparameter *search-path-pindex*
  (make-array '(13)
	      :element-type 'maybe-pindex
	      :initial-element -1)
  "current path to a solution")
(declaim (type (simple-array maybe-pindex (13)) *search-path-pindex*))
(defparameter *search-path-shape*
  (make-array '(13)
	      :element-type 'encoded-shape
	      :initial-element (null-encoded-shape))
  "current path to a solution")
(declaim (type (simple-array encoded-shape (13)) *search-path-shape*))
(defparameter *search-path-remaining-space*
  (make-array '(14)
	      :element-type t
	      :initial-element '())
  "current path to a solution")
(declaim (type (simple-array t (14)) *search-path-remaining-space*))
(defparameter *search-depth* 13
  "remaining depth so search")
(declaim (type (integer 0 13) *search-depth*))
(defparameter *min-depth* 13
  "maximum depth reach during the search (debugging only)")
(declaim (type (integer 0 13) *min-depth*))
(defparameter *search-spot* 63
  "current spot to search")
(declaim (type (integer 0 64) *search-spot*))
(defparameter *search-cube* (null-encoded-shape)
  "current cube to search")
(declaim (type encoded-shape *search-cube*))
(defparameter *failures* 0
  "total number of failed attempts so far")
(declaim (type integer *failures*))
(defparameter *failures-before-next-check* 0
  "number of failed attempts to run before next check")
(declaim (type fixnum *failures-before-next-check*))
(defparameter *failures-between-checks* 100
  "number of failed attempts between two checks")
(declaim (type fixnum *failures-between-checks*))
(defparameter *start-time* 0
  "time at which search started")
(defparameter *seconds-per-check* 100)
(defparameter *last-check-time* 0)
(defparameter *etrshapes-file*
  "etrshapes8.lisp"
  "file where to save a cached,
   precomputed version of TRANSLATED-ROTATED-SHAPES")
(defparameter *save-search-file*
  (format nil "search-~X.lisp" *search-tree-id*)
  "file containing data to restart the search from")
(defparameter *restart-with* '()
  "data explaining where to restart the search from")

;;; Help the compiler make things faster

;;; Fast routines for fixnum
(defsubst fixnum+ (a b)
  "add two fixnums, fast"
  #+declare-types (declare (type fixnum a b))
  (the* fixnum (+ a b)))
(declaim (ftype (function (fixnum fixnum) fixnum) fixnum+))
(defsubst fixnum- (a b)
  "substract two fixnums, fast"
  #+declare-types (declare (type fixnum a b))
  (the* fixnum (- a b)))
(declaim (ftype (function (fixnum fixnum) fixnum) fixnum-))
(defsubst fixnum-logand (a b)
  "substract two fixnums, fast"
  #+declare-types (declare (type fixnum a b))
  (the* fixnum (logand a b)))
(declaim (ftype (function (fixnum fixnum) fixnum) fixnum-logand))

;;; Routines to deal with encoding
(defsubst piece->pindex (piece)
  "get the pindex for PIECE"
  #+declare-types (declare (type piece piece))
  (1- piece))
(declaim (ftype (function (piece) (integer 0 12)) piece->pindex))

(defsubst pindex->piece (pindex)
  "get the piece for PINDEX"
  #+declare-types (declare (type pindex pindex))
  (1+ pindex))

(defsubst point->cindex (point)
  "encode CUBE"
  #+declare-types (declare (type point point))
  (apply #'array-row-major-index *cube* point))
(declaim (ftype (function (point) (or cindex null)) point->cindex))

(defsubst simple-shape->cil-shape (shape)
  "encode SHAPE"
  #+declare-types (declare (type simple-shape shape))
  (mapcar #'point->cindex shape))
(declaim (ftype (function (simple-shape) cil-shape) simple-shape->cil-shape))

(defsubst cil-shape->bitmask-shape (shape)
  "transform encoded SHAPE into bitmask"
  #+declare-types (declare (type cil-shape shape))
  (loop for cindex of-type cindex in shape
    sum (ash 1 cindex)))
(declaim (ftype (function (cil-shape) bitmask-shape) cil-shape->bitmask-shape))

(defsubst bitmask-shape->cil-shape (bshape)
  "transform encoded SHAPE into bitmask"
  #+declare-types (declare (type bitmask-shape bshape))
  (loop
    for cindex of-type fixnum from 0 below =cubes=
    for mask = (ash 1 cindex)
    unless (zerop (logand bshape mask))
    collect cindex))
(defconstant =xbits= 32)
(defconstant =xmask= (1- (ash 1 =xbits=)))
(defsubst bitmask-shape->xbitmask-shape (shape)
  #+declare-types (declare (type bitmask-shape shape))
  (make-xbitmask-shape (logand =xmask= shape) (ash shape (- =xbits=))))
(declaim (ftype (function (bitmask-shape) xbitmask-shape) bitmask-shape->xbitmask-shape))

(defsubst xbitmask-shape->bitmask-shape (shape)
  #+declare-types (declare (type xbitmask-shape shape))
  (+ (xbitmask-shape-lo shape)
     (ash (xbitmask-shape-hi shape) =xbits=)))
(declaim (ftype (function (xbitmask-shape) bitmask-shape) xbitmask-shape->bitmask-shape))

(defsubst cil-shape-spot (s)
  (car (last s)))
(defsubst bitmask-shape-spot (s)
  (1- (integer-length s)))
(defsubst xbitmask-shape-spot (s)
  (bitmask-shape-spot (xbitmask-shape->bitmask-shape s)))
(defsubst shape-spot (s)
  #+cil (cil-shape-spot s)
  #+bitmask (bitmask-shape-spot s)
  #+xbitmask (xbitmask-shape-spot s))
(defun encode-shape (s)
  #+cil (simple-shape->cil-shape s)
  #+bitmask (cil-shape->bitmask-shape (simple-shape->cil-shape s))
  #+xbitmask (bitmask-shape->xbitmask-shape
	      (cil-shape->bitmask-shape
	       (simple-shape->cil-shape s))))
(defun shape-sort-index (s)
  #+cil (cil-shape->bitmask-shape s)
  #+bitmask s
  #+xbitmask (xbitmask-shape->bitmask-shape s))
(defsubst encoded-shape->cil-shape (s)
  #+cil s
  #+bitmask (bitmask-shape->cil-shape s)
  #+xbitmask (bitmask-shape->cil-shape (xbitmask-shape->bitmask-shape s)))
(declaim (ftype (function (encoded-shape) cil-shape) encoded-shape->cil-shape))
(defsubst cil-shape->encoded-shape (s)
  #+cil s
  #+bitmask (cil-shape->bitmask-shape s)
  #+xbitmask (bitmask-shape->xbitmask-shape (cil-shape->bitmask-shape s)))
(defsubst cil-shape-compatible-p (s1 s2)
  "do encoded shapes S1 and S2 intersect?"
  #+declare-types (declare (type cil-shape s1 s2))
  (block compatible-p
    (loop for c1 in s1 do
      (loop for c2 in s2 do
	(if (= (the* cindex c1) (the* cindex c2))
	    (return-from compatible-p nil))))
    t))
(defsubst bitmask-shape-compatible-p (s1 s2)
  "do bitmask shapes S1 and S2 intersect?"
  #+declare-types (declare (type bitmask-shape s1 s2))
  (zerop (logand s1 s2)))
(defsubst xbitmask-shape-compatible-p (s1 s2)
  "do xbitmask shapes S1 and S2 intersect?"
  #+declare-types (declare (type xbitmask-shape s1 s2))
  (and (zerop (logand (xbitmask-shape-lo s1) (xbitmask-shape-lo s2)))
       (zerop (logand (xbitmask-shape-hi s1) (xbitmask-shape-hi s2)))))
(defsubst shape-compatible-p (s1 s2)
  "are encoded shapes S1 and S2 compatible?"
  #+declare-types (declare (type encoded-shape s1 s2))
  #+cil (cil-shape-compatible-p s1 s2)
  #+bitmask (bitmask-shape-compatible-p s1 s2)
  #+xbitmask (xbitmask-shape-compatible-p s1 s2))
(defsubst shape-valid-p (s)
  (shape-compatible-p *search-cube* s))
(defsubst bitmask-shape-highest-free-spot (shape)
  #+declare-types (declare (type bitmask-shape shape))
  (the* (integer -1 63)
    (1- (integer-length (logxor shape #xFFFFFFFFFFFFFFFF)))))
(defsubst ub32-not (x)
  #+declare-types (declare (type ub32 x))
  (the* ub32 (logxor x #xFFFFFFFF)))
(defsubst xbitmask-shape-highest-free-spot (shape)
  #+declare-types (declare (type xbitmask-shape shape))
  (the* (integer -1 63)
    (let ((hi (ub32-not (xbitmask-shape-hi shape))))
      (if (zerop hi)
	  (1- (integer-length (ub32-not (xbitmask-shape-lo shape))))
	(fixnum+ 31 (integer-length hi))))))
(TEST-FORMS
 (loop for i from 0 to 63 for b = (- (ash 1 64) (ash 1 i))
   for x = (bitmask-shape->xbitmask-shape b)
   for bs = (bitmask-shape-highest-free-spot b)
   for xs = (xbitmask-shape-highest-free-spot x)
   ;do (MSG "~% ~64,0B ~D ~D ~D" b (1- i) bs xs)
   always (= (1- i) bs xs)) t)

(defmacro next-spot ()
  #+bitmask '(bitmask-shape-highest-free-spot *search-cube*)
  #+xbitmask '(xbitmask-shape-highest-free-spot *search-cube*))
(define-modify-macro logxorf (&rest args) logxor)
(defmacro bitmask-shape-xor! (sloc sinc)
  `(logxorf ,sloc ,sinc))
(defmacro xbitmask-shape-xor! (sloc sinc)
  `(progn
     (logxorf (xbitmask-shape-hi ,sloc) (xbitmask-shape-hi ,sinc))
     (logxorf (xbitmask-shape-lo ,sloc) (xbitmask-shape-lo ,sinc))))
(defmacro shape-xor! (sloc sinc)
  #+bitmask `(bitmask-shape-xor! ,sloc ,sinc)
  #+xbitmask `(xbitmask-shape-xor! ,sloc ,sinc))
(defmacro define-enclosing-macro (name args before after &key unprotected)
  `(defmacro ,name (,@args &body body)
     `(progn
	,,before
	(,,(if unprotected ''prog1 ''unwind-protect)
	,@body
	,,after))))
(define-enclosing-macro with-shape-merged (sloc sinc)
  `(shape-xor! ,sloc ,sinc)
  `(shape-xor! ,sloc ,sinc))

;;; Routines to deal with 3D coordinates
(defsubst translate-point (point translation)
  "translate POINT by TRANSLATION -- fast"
  #+declare-types (declare (type point point translation))
  (list (fixnum+ (first point) (first translation))
	(fixnum+ (second point) (second translation))
	(fixnum+ (third point) (third translation))))
(declaim (ftype (function (point point) point) translate-point))

(defsubst point-difference (point1 point2)
  "find TRANSLATION from POINT2 to POINT1 -- fast"
  #+declare-types (declare (type point point1 point2))
  (declare (ftype (function (point point) point) point-difference))
  (list (fixnum- (first point1) (first point2))
	(fixnum- (second point1) (second point2))
	(fixnum- (third point1) (third point2))))
(declaim (ftype (function (point point) point) point-difference))

(defsubst oppose-point (point)
  "find the opposite of POINT in 3D piece"
  (point-difference '(0 0 0) point))
(declaim (ftype (function (point) point) opposite-point))

(defsubst translate-shape (shape translation)
  "translate SHAPE by TRANSLATION"
  (mapcar #'(lambda (point) (translate-point point translation)) shape))
(declaim (ftype (function (simple-shape point) point) translate-shape))

(TEST-FORMS
 (translate-shape '((2 1 3) (1 2 3) (4 5 6) (3 3 4)) '(-1 -2 -3))
  '((1 -1 0) (0 0 0) (3 3 3) (2 1 1)))



(defsubst row-major-compare-point (point1 point2)
  ; auxiliary function used when normalizing shapes
  "compare (<) two lists in lexicographic (row-major) order."
  (loop
    for c1 of-type fixnum in point1
    for c2 of-type fixnum in point2
    when (< c1 c2) return t
    else when (> c1 c2) return nil
    finally (return nil)))
(declaim (ftype (function (point point) boolean) row-major-compare-point))

(TEST-FORMS
 (row-major-compare-point '(1 2 3) '(1 2 3)) nil
 (row-major-compare-point '(1 2 3) '(1 2 2)) nil
 (row-major-compare-point '(1 2 3) '(1 2 4)) t
 (row-major-compare-point '(1 2 3) '(2 2 4)) t
 (row-major-compare-point '(1 2 3) '(0 2 4)) nil)

;;; Manipulating shapes
(defsubst reduce-coordinates (shape fun)
  "reduce coordinates for SHAPE"
  (reduce #'(lambda (point1 point2) (mapcar fun point1 point2)) shape))
(declaim (ftype (function (simple-shape function) point) reduce-coordinates))

(defsubst min-coordinates (shape)
  "min coordinates for SHAPE"
  (reduce-coordinates shape #'min))
(declaim (ftype (function (simple-shape) point) min-coordinates))

(defsubst max-coordinates (shape)
  "max coordinates for SHAPE"
  (reduce-coordinates shape #'max))
(declaim (ftype (function (simple-shape) point) max-coordinates))

(defsubst normalize-shape (shape)
  "put SHAPE in normal form:
all its minimal coordinates being 0,
and its cubes ordered in lexicographic (row-major) order."
  (let* ((min-coord (min-coordinates shape))
	 (translated-shape
	  (translate-shape shape (oppose-point min-coord))))
    (sort translated-shape #'row-major-compare-point)))
(declaim (ftype (function (simple-shape) simple-shape) normalize-shape))
(TEST-FORMS
 (normalize-shape '((2 1 3) (1 2 3) (4 5 6) (3 3 4)))
 '((0 1 0) (1 0 0) (2 2 1) (3 4 3)))

(defsubst compare-shapes (s1 s2)
  "compare if normalized shapes S1 and S2 are equal"
  (equal s1 s2))
(declaim (ftype (function (simple-shape simple-shape) boolean) compare-shape))

(defsubst row-major-compare-shapes (s1 s2)
  "compare (<) normalized shapes S1 and S2 in lexicographic (row-major) order."
  (loop for p1 in s1 for p2 in s2
    when (row-major-compare-point p1 p2) return t
    else when (row-major-compare-point p2 p1) return nil
    finally (return nil)))
(declaim (ftype (function (simple-shape simple-shape) boolean) row-major-compare-shape))

(defsubst rotate-shape (shape rotation)
  "rotate SHAPE according to ROTATION"
  (normalize-shape (mapcar rotation shape)))
(declaim (ftype (function (simple-shape function) simple-shape) rotate-shape))


;;; Generating shapes
(defsubst shape-rotations (shape)
  "find all *different* rotated positions for SHAPE"
  (loop
    with positions = '()
    for rotation in =rotations=
    do (pushnew (rotate-shape shape rotation) positions
		:test #'compare-shapes)
    finally (return positions)))
(declaim (ftype (function (simple-shape) list) shape-rotations))

(defsubst shape-translations (shape)
  "find all different translated positions for *normalized* SHAPE"
  (loop
    with tshapes = '()
    with max-coord = (max-coordinates shape)
    with max-x = (first max-coord)
    with max-y = (second max-coord)
    with max-z = (third max-coord)
    for x of-type fixnum below (- =max-coordinate= max-x) do
    (loop for y of-type fixnum below (- =max-coordinate= max-y) do
      (loop for z of-type fixnum below (- =max-coordinate= max-z)
	do (push (translate-shape shape (list x y z)) tshapes)))
    finally (return tshapes)))
(declaim (ftype (function (simple-shape) list) shape-translations))

(defsubst shape-rotations-translations (shape)
  "find all *different* rotated translated positions for SHAPE"
  (loop for rshape in (shape-rotations shape)
    nconc (shape-translations rshape)))
(declaim (ftype (function (simple-shape) list) shape-rotations-translations))

(defsubst cil-shape-rotations-translations (shape)
  "find and encode all rotated translated positions for SHAPE"
  (let* ((rtshape (shape-rotations-translations shape))
	 (crtshape (mapcar #'simple-shape->cil-shape rtshape)))
    (sort crtshape #'< :key #'cil-shape->bitmask-shape)))


;;; Initializing the search
(defun precompute-trshapes ()
  (MSG "~%Precomputing translated rotated shapes...")
  (DBG-TIME
   (setf translated-rotated-shapes
	 (mapcar #'cil-shape-rotations-translations =piece-list=))))
(defun use-precomputed-trshapes ()
  "use precomputed TRANSLATED-ROTATED-SHAPES from file"
  (with-open-file (s *etrshapes-file* :direction :input :if-does-not-exist nil)
    (and s (setf translated-rotated-shapes (read s)))))
(defun save-trshapes ()
  "save TRANSLATED-ROTATED-SHAPES"
  (with-open-file (s *etrshapes-file* :direction :output :if-exists nil)
    (and s (write translated-rotated-shapes :stream s))))
(defun precompute-and-save-trshapes ()
  "precompute and save TRANSLATED-ROTATED-SHAPES"
  (precompute-trshapes)
  (save-trshapes))
(defun initialize-translated-rotated-shapes ()
  "initialize TRANSLATED-ROTATED-SHAPES"
  (or (use-precomputed-trshapes)
      (precompute-and-save-trshapes))
  (fare-utils::do-test-form ; sanity check on the result of initialization
   (mapcar #'length translated-rotated-shapes)
	 '(432 216 216 324 192 432 432 432 48 432 432 216 384)
	 ))
(defun initialize-initial-search-shapes ()
  "sort shapes according to preference"
  (setf initial-search-shapes
	(loop
	  for shape-list in translated-rotated-shapes
	  for i from 0
	  collect (list i (length shape-list)
			(mapcar #'cil-shape->encoded-shape shape-list)))))
(defun prepare-search-shapes (search-shapes)
  "Prepare pieces according to their shapes"
  (loop
    for (pindex nil shape-list) in search-shapes
    collect
    (loop
      with table = (make-array '(64) :element-type 'list :initial-element nil)
      for shape in shape-list
      do (push shape (aref table (shape-spot shape)))
      finally (return (make-piece-shapes
		       :pindex pindex
		       :spotted-shapes-vector table)))))
(defun prepare-search ()
  "prepare for search: precompute stuff"
  (MSG "~%Preparing search...")
  (initialize-translated-rotated-shapes)
  (initialize-initial-search-shapes))

(defun initialize-search ()
  "initialize search space"
  (MSG "~%Initializing search space...")
  (fill-array *cube* 0)
  (fill-array *search-path-pindex* -1)
  (fill-array *search-path-shape* (null-encoded-shape))
  (fill-array *search-path-remaining-space* '())
  (setq	*min-depth* 13
	*failures* 0
	*failures-before-next-check* *failures-between-checks*
	*search-depth* 13
	*search-shapes* initial-search-shapes
	*start-time* (get-internal-run-time)
	*last-check-time* *start-time*
	*solutions* '())
	*solutions-count* 0)

;;; Success story
(defsubst current-depth ()
  "current depth of the search"
  (- 13 *search-depth*))
(defun current-path ()
  "current search path"
  (loop
    with path-pindex = (copy-array *search-path-pindex*)
    with path-shape = (copy-array *search-path-shape*)
    for i from (1- *search-depth*) downto 0 ; mind the 1-
    do (setf (aref path-pindex i) -1
	     (aref path-shape i) (null-encoded-shape))
    finally (return (cons path-pindex path-shape))))
(defun make-solution (paths)
  "current solution in better form"
  (loop
    with path-pindex = (car paths)
    with path-shape = (cdr paths)
    with *cube* = (empty-cube)
    for pindex across path-pindex
    for shape across path-shape
    do (loop
	 for cindex in (encoded-shape->cil-shape shape) do
	 (setf (row-major-aref *cube* cindex) (pindex->piece pindex)))
    finally (return *cube*)))
(defun show-solutions (&optional (solutions *solutions*))
  (format t "~%Solutions:~%~{~%~A~%~}~%Found ~D solutions."
	  solutions (length solutions)))
(defun current-solution ()
  "state of the solution found: the cube"
  (make-solution (current-path)))
(defun show-cube-hex (cube)
  (let ((*print-base* 16)) (format t "~%~A" cube)))
(defun show-cube-dec (cube)
  (loop
    initially (format t "~%(")
    for i from 0 to 3
    do (loop
	 initially (format t "(")
	 for j from 0 to 3
	 do (loop
	      initially (format t "(")
	      for k from 0 to 3
	      do (format t "~2D" (aref cube i j k)) ; "~1X"
	      when (< k 3) do (format t " ")
	      finally (format t ")"))
	 when (< j 3) do (format t " ")
	 finally (format t ")"))
    when (< i 3) do (format t "~% ")
    finally (format t ")")))
(defun show-cube (cube)
  (show-cube-dec cube))
(defun cube ()
  "current solution in better form"
  (show-cube (current-solution)))
(defun pieces-search-size-and-advancement (pieces pindex shape)
  #+declare-types (declare (type pindex pindex) (type encoded-shape shape))
  (let* ((size
	  (reduce #'+
		  (mapcar #'length
			  (mapcar
			   #'(lambda (piece)
			     ;(remove-if-not #'shape-valid-p
				     (piece-shapes-spotted-shapes
				      piece *search-spot*));)
			   pieces))))
	 (advancement
	  (loop
	    for piece in pieces
	    for shapes = ;(remove-if-not #'shape-valid-p
				 (piece-shapes-spotted-shapes
				  piece *search-spot*);)
	    for px = (piece-shapes-pindex piece)
	    when (= pindex px)
	      return (+ sum (or (position shape shapes) 0))
	    else
	      sum (length shapes) into sum
	    finally (return sum))))
    #+XXX
    (MSG "~%size=~D advancement=~D spot=~D cube=~B pindex=~D shape=~A"
	 size advancement *search-spot* *search-cube*
	 pindex (encoded-shape->cil-shape shape))
    (values size advancement)))
(defun show-path ()
  (cube)
  (let ((done 0) (total 1) (restart-from '())
	(*search-cube* (null-encoded-shape))
	(*search-spot* 63))
    (loop named walk-path
      for i from =max-pindex= downto *search-depth* do
      (let* ((pindex (aref *search-path-pindex* i))
	     (shape (aref *search-path-shape* i))
	     (pieces (aref *search-path-remaining-space* (1+ i)))
	     (piece (pindex->piece pindex))
	     (cil (encoded-shape->cil-shape shape)))
	(if (equal shape (null-encoded-shape))
	    (return-from walk-path))
	  (multiple-value-bind
	   (size advancement)
	   (pieces-search-size-and-advancement pieces pindex shape)
	   (if (zerop size) (return-from walk-path))
	   (setf *search-spot* (next-spot))
	   (shape-xor! *search-cube* shape)
	   (setf done (+ advancement (* done size))
		 total (* total size))
	   (push advancement restart-from)
	   (format t "~%:spot ~2D :advancement ~3D/~3D :piece ~2D :shape ~A"
		   *search-spot* advancement size piece cil))))
    (let* ((advancement (coerce (/ done total) 'long-float))
	   (termination (/ (- (get-internal-run-time) *start-time*)
			  internal-time-units-per-second
			  (max advancement 1.e-20)))
	   (total-elapsed-time
	    (coerce (/ (- (get-internal-run-time) *start-time*)
		       internal-time-units-per-second)
		    'float))
	   (restart-with
	    (list
	     'r-s-w
	      *search-tree-id*
	      *failures*
	      total-elapsed-time
	      (reverse restart-from)
	      *solutions-count*)))
      (setf *restart-with* restart-with)
      ;(do-save-search-state)
      (format t "~%Estimated advancement: ~D (~D/~D)~%~
                 Estimated termination: ~D seconds~%~W~%"
	      advancement done total termination restart-with))))
(defun path ()
  "compute the current path"
  (show-path))
(defun floor-to-adjusted-power
  (x &optional (n 10) (adjustment-factors '(1 2.5 5)) (minimum 1))
  (let* ((v (max x minimum))
	 (p (floor (log v n)))
	 (m (expt n p))
	 (r (/ v m))
	 (f (or (find r adjustment-factors :test #'>= :from-end t) 1)))
    (* f m)))
(defun failures (show-p)
  (let* ((previous-failures *failures*)
	 (previous-time *last-check-time*)
	 (current-time (get-internal-run-time))
	 (elapsed-time
	  (coerce (/ (- current-time previous-time)
		     internal-time-units-per-second)
		  'float))
	 (total-elapsed-time
	  (coerce (/ (- current-time *start-time*)
		     internal-time-units-per-second)
		  'float))
	 (failures-done
	  (- *failures-between-checks* *failures-before-next-check*))
	 (new-failures (+ previous-failures failures-done))
	 (check-frame-factor
	  (/ (max 1.e-8 elapsed-time) *seconds-per-check*))
	 (failures-per-check-frame
	  (/ failures-done check-frame-factor))
	 (adjusted-failures-per-check-frame
	  (floor
	   (max 100
		(floor-to-adjusted-power
		 (min most-positive-fixnum failures-per-check-frame)))))
	 (next-check-frame
	  (if (< 1.e-1 elapsed-time)
	      (+ new-failures *failures-between-checks*)
	    (ceiling
	     (* adjusted-failures-per-check-frame
		(1+ (ceiling
		     (1+ new-failures) adjusted-failures-per-check-frame))))))
	 (failures-before-next-check
	    (- next-check-frame new-failures)))
    (setf *failures* new-failures
	  *failures-between-checks* failures-before-next-check
	  *failures-before-next-check* failures-before-next-check
	  *last-check-time* current-time)
    (when show-p
      (format t "~%~D attempts" *failures*)
      (format t "	:failures-done ~D ~%~
				:elapsed-time ~D ~%~
				:total-elapsed-time ~D ~%~
				:new-failures ~D ~%~
				:check-frame-factor ~D ~%~
				:failures-per-second ~9,2E ~%~
				:adjusted-failures-per-check-frame ~D ~%~
				:next-check-frame ~D ~%~
				:failures-before-next-check ~D"
	      failures-done
	      elapsed-time
	      total-elapsed-time
	      new-failures
	      check-frame-factor
	      (/ failures-per-check-frame *seconds-per-check*)
	      adjusted-failures-per-check-frame
	      next-check-frame
	      failures-before-next-check
	      ))
    *failures*))
(defsubst success ()
  "what to do when we find a solution: save it"
  #+xxx (failures t)
  (format t
	  #+do-debug "~%Found solution #~D after ~D attempts!"
	  #-do-debug "~%Found solution #~D!"
	  (incf *solutions-count*)
	  #+do-debug *failures*)
  #+(or do-debug do-test verbose)
   (path)
  #+(and (or cmu sbcl) do-debug)
   (when (zerop (- (rem *solutions-count* 20) 15))
     (#+cmu extensions:gc #+sbcl sb-ext:gc :full t :verbose t))
  (push (current-solution) *solutions*))

;;; Searching for a solution: trying all ways to allocate resources

(defmacro with-shape-added-to-cube (&body body)
  `(with-shape-merged *search-cube* shape ,@body))
(defmacro account-failure ()
  #-do-debug nil #+do-debug
  '(when (zerop (decf *failures-before-next-check*))
     (failures
        #+(or do-debug do-test accounting) t
        #-(or do-debug do-test accounting) nil)
     (path)))
(defmacro when-valid-shape (&body body)
  `(if (shape-valid-p shape)
       (progn ,@body)
     #+(or do-debug do-test accounting)
     (account-failure)))
(define-enclosing-macro with-piece-and-shape-added-to-solution ()
  '(progn
    #+XXX (MSG "~%depth=~D pindex=~D shape=~A" *search-depth* pindex shape)
    (setf (aref *search-path-pindex* (1- *search-depth*)) pindex)
    (setf (aref *search-path-shape* (1- *search-depth*)) shape)
    (setf (aref *search-path-remaining-space* (1- *search-depth*))
	*search-shapes*)
    (decf *search-depth*))
  '(incf *search-depth*))
(defmacro try-harder ()
  '(if (successp)
       (success)
     (search-with-remaining-pieces)))
(defmacro successp ()
  '(zerop *search-depth*))
(defmacro search-with-remaining-pieces ()
  '(let ((*search-spot* (next-spot)))
     (search-solution)))
(defmacro try-shape ()
  '(with-shape-added-to-cube
    (with-piece-and-shape-added-to-solution
     (try-harder))))

(defun search-solution ()
  (dolist-with-rest
   (piece *search-shapes*) *search-shapes*
   (let ((pindex (piece-shapes-pindex piece)))
     (dolist (shape (piece-shapes-spotted-shapes piece *search-spot*))
       #+XXX (MSG "~%Depth=~D spot=~D pindex=~D shape=~A"
		  *search-depth* *search-spot* pindex shape)
       (when-valid-shape
	(try-shape))))))

;;; Speeding up the search by breaking the symmetry

(defun search-with-cross-first ()
  "search, breaking the symmetry by first inserting the cross piece (9)
in horizontal position, in the lower front left part of the cube."
  (MSG "~%Searching with cross first...")
  (let*
      ((pindex 8)
       (cross-shape (nth pindex =piece-list=))
       (cross-shapes
	(mapcar #'(lambda (rot trans)
		    (encode-shape (translate-shape
				   (rotate-shape cross-shape rot)
				   trans)))
		(list (mkrot (x y z)) (mkrot (z x y)))
		'((1 1 2) (3 1 1))))
       (*search-shapes* (prepare-search-shapes
			 (remove-nth pindex
				     initial-search-shapes)))
       (piece (make-piece-shapes :pindex pindex)))
    (setf (piece-shapes-spotted-shapes piece 63) cross-shapes)
    (setf (aref *search-path-remaining-space* *search-depth*) (list piece))
    #+XXX (MSG "~%First-level space: ~A" (list piece))
    (loop for shape in cross-shapes do
      (try-shape))))

(defun search-with-broken-symmetry ()
  "break the symmetry before to search, so as to reduce the search space"
  (MSG "~%Beginning search in reduced space...")
  (initialize-search)
  (DBG-TIME
   (search-with-cross-first)))

;;; Recovering the full problem by applying symmetries to solutions found

(defun transform-solution (solution symmetry)
  "recover other solutions "
  (let ((symmetric-solution (copy-array-shape solution)))
    (loop for n below =cubes= do
      (let* ((coords (aref =cube-unflattener= n))
	     (sym-coords (apply symmetry coords)))
	(setf (apply #'aref symmetric-solution sym-coords)
	      (apply #'aref solution coords))))
    symmetric-solution))
(defun symmetrize-solution (solution)
  (mapcar #'(lambda (symmetry) (transform-solution solution symmetry))
	  =rotations=))
(defun symmetrize-solutions ()
  "find all solutions by applying symmetries to those found by brute force"
  (setf *solutions*
	(mapcan #'symmetrize-solution *solutions*)))


;;; Solving the problem

(defun solve-problem ()
  "Solve the bloody problem"
  (prepare-search)
  (search-with-broken-symmetry)
  #| ; uncomment to have all solutions, not just those in normal form
  (symmetrize-solutions)
  ;for prettier output, sort and unify solutions
  |#
  (MSG "~%Done! (min depth ~D)~%" *min-depth*)
  *solutions*)

; Note: the ultimate LISP optimization would be to use defconstant here,
; and to have all the previous stuff done at compile time.
(defparameter
  bedlam-solutions
  (time (solve-problem))) ; just do it!

(show-solutions bedlam-solutions)

T
