#|;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
':' 1 ; cat <<'Solving a simple problem from ITA'>> /dev/null
; Solution proposed by fare@itasoftware.com
; You can run this program with the command:
	sh fare.lisp
; or give it an explicit argument such as
	sh fare.lisp 1234567890
	sh fare.lisp '(expt 10 1111)'
"
Lucky Sevens

Write a program to compute the sum of all the integers between 1 and 10^11
that are both divisible by seven and, when the decimal digits are reversed,
are still divisible by seven.

" "
This solution solves the general case of efficiently summing the "lucky sevens" up to N
for an arbitrary bignum values of N. It works particularly well when N is sparse, that is,
has few non-zero digits. You can easily compute sum the lucky sevens up to (+ 42 (expt 10 10000)).

This solution is based on the fast matrix exponentiation algorithm I explained
in my program to compute Fibonacci numbers:
	http://www.livejournal.com/users/fare/59015.html
The "matrices" considered here basically express for a given number of consecutive digits
how many combinations of those digits there are that have this or that remainder modulo 7
*and* this or that remainder modulo 7 when reversed, as well as the sum of the numbers that
match the criteria. The matrices are combined by taking into account the fact that the
remainder is multiplied by (expt 10 n) modulo 7 when shifted n places, and doing the right
thing concerning the sum.
"|#
(in-package :cl-user)

(defparameter *max* (expt 10 11)
  "The requested number up to which the count is done.")
(defparameter *approximate-solution*
  (floor (* 1/2 (+ 1 *max*) *max* 1/49))
  "a good approximation of the solution, considering that about one in 49 integers fits the bill")

(defmacro dbg (tag &rest vars)
  "simple debug statement macro:
outputs a tag plus a list of variable and their values"
  `(format t "~%~A ~A~%" ',tag `(,,@(mapcan #'(lambda (x) `(',x ,x)) vars))))


;;; arithmetic modulo 7
(defun %7 (x) (mod x 7))
(defun *%7 (x y) (%7 (* x y)))
(defun +%7 (x y) (%7 (+ x y)))
(defun -%7 (x y) (%7 (- x y)))
(defun square%7 (x) (*%7 x x))
(defun expt%7 (x n)
  (%7 (expt (%7 x) (mod n 6))))
(defun inv%7 (x)
  (expt%7 x -1))
(defun opp%7 (x)
  (%7 (- x)))
(defconstant +10%7+ 3)
(defconstant +1/10%7+ 5)


;;; array to hold how many numbers in each state (modulo 7, reverse modulo 7)
;;; there are between 0 (included) and 10 to the n (excluded),
;;; and how much they add up to.
(defun make-sol-array ()
  (make-array '(7 7 2) :element-type 'integer :initial-element 0))
(defun sums0 ()
  (let ((s (make-sol-array)))
    (setf (aref s 0 0 0) 1)
    s))
(defun sums1 ()
  (loop with s = (make-sol-array)
       for n below 10
       for i = (%7 n) do
       (incf (aref s i i 0) 1)
       (incf (aref s i i 1) n)
       finally (return s)))
(defvar *sums0* (sums0))
(defvar *sums1* (sums1))

;;; special case of diagonal arrays for digits up to d
(defun make-sum-to ()
  (loop with s = (make-array '(10 7 2) :initial-element 0)
	initially (setf (aref s 0 0 0) 1)
	for i from 1 to 9 do
	(loop for j below 7 do
	      (let ((a0 (aref s (1- i) j 0))
		    (a1 (aref s (1- i) j 1))
		    (b (= (%7 i) j)))
	      (setf (aref s i j 0) (if b (1+ a0) a0)
		    (aref s i j 1) (if b (+ a1 i) a1))))
	finally (return s)))
(defvar *sum-to* (make-sum-to))

(defun combine-terms (a0 a1 af b0 b1)
  "if there are A0 terms summing to A1 in the lower digits,
combined with B0 terms summing to B1 in the higher digits,
with AF being 10 to the number of lower digits,
how many combinations does that make that sum to how much?"
;;; This associative operator plus the generator for a 1-digit choice
;;; makes for a commutative quasi-group.
;;; (actually, you'd also have to keep around generate F = (* AF BF))
  (values (* a0 b0) (+ (* a1 b0) (* a0 b1 af))))

(defun combine-sums (x y n m)
  (loop
     with f = (expt%7 10 n)
     with g = (expt%7 10 m)
     with ff = (expt 10 n)
     with z = (make-sol-array)
     for i below 7 do
       (loop for j below 7 do
	    (loop for k below 7
		 for s = (+%7 i (*%7 f k)) do
		 (loop for l below 7
		    for r = (+%7 (*%7 g j) l) do
		    (multiple-value-bind (c0 c1)
			(combine-terms (aref x i j 0) (aref x i j 1) ff
				       (aref y k l 0) (aref y k l 1))
		      (incf (aref z s r 0) c0)
		      (incf (aref z s r 1) c1)))))
       finally (return z)))

(defun combine-sum-1 (y m)
  ;;;should be same as (combine-sums *sums1* 1 y m)
  (combine-sum-to 9 y m))

(defun combine-sum-to (d y m)
  (loop
     with f = 3 ; (expt%7 10 1)
     with g = (expt%7 10 m)
     with ff = 10 ; (expt 10 1)
     with z = (make-sol-array)
     for i below 7 do
	    (loop for k below 7
		 for s = (+%7 i (*%7 f k)) do
		 (loop for l below 7
		    for r = (+%7 (*%7 g i) l) do
		    (multiple-value-bind (c0 c1)
			(combine-terms
			 (aref *sum-to* d i 0) (aref *sum-to* d i 1) ff
			 (aref y k l 0) (aref y k l 1))
		      (incf (aref z s r 0) c0)
		      (incf (aref z s r 1) c1))))
       finally (return z)))

(defun sums (n)
  (cond
    ((= n 0) *sums0*)
    ((= n 1) *sums1*)
    (t (let* ((m (ash n -1))
	      (h (sums m))
	      (s (combine-sums h h m m)))
	 (if (oddp n) (combine-sum-1 s (1- n)) s)))))


;;; A very simple and generic memoizing function
(defun unmemoize (sym)
  "undoing the memoizing function, return the hash of memoized things so far"
  (let ((r (get sym :original-memoized-function)))
    (when r
      (setf (symbol-function sym) (car r))
      (remprop sym :original-memoized-function)
      (cdr r))))
(defun compute-memoized-function (f h args)
  "the basic helper for computing with a memoized function F,
with a hash-table H, being called with arguments ARGS"
  (multiple-value-bind (results foundp) (gethash args h)
    (if foundp (apply #'values results)
	(let ((results (multiple-value-list (apply f args))))
	  (setf (gethash args h) results)
	  (apply #'values results)))))
(declare (inline compute-memoized-function))
(defun memoize (sym &optional (h (make-hash-table :test 'equal)))
  "a pretty generic memoizing function to speed things up"
  (unmemoize sym)
  (let ((f (symbol-function sym)))
    (setf (symbol-function sym)
	  #'(lambda (&rest args)
	      (compute-memoized-function f h args))
	  (get sym :original-memoized-function)
	  (cons f h))))
(defmacro define-memo-function (name formals &body body)
  `(progn (defun ,name ,formals ,@body) (memoize ',name)))
(defun memoizing (f)
  (let ((h (make-hash-table :test 'equal)))
    #'(lambda (&rest args)
	(compute-memoized-function f h args))))
(defmacro memo-lambda (formals &body body)
  `(memoizing #'(lambda ,formals ,@body)))
(memoize 'sums) ;;; trade logarithmic time for logarithmic space


;;; The cheap way to compute the solution: (clever-sum 11)
(defun clever-sum (n)
  (aref (sums n) 0 0 1))

(defun reverse-number (n)
  (loop
     with r = 0
     while (> n 0)
       do (multiple-value-bind (q d) (floor n 10)
	    (setf r (+ d (* 10 r))
		  n q))
       finally (return r)))

(defun number-length (n)
  (loop
     for x = n then (floor x 10)
     while (> x 0)
       sum 1))
;(defun number-length (n) (nth-value 0 (ceiling (log (1+ n) 10))))

(defun dumb-count (n)
  (loop for i from 1 to n
       when (= 0 (%7 i) (%7 (reverse-number i)))
       sum i))

(defun partial-sum (h lh d q lq q7 r7 ff)
  "Counting good numbers above or equal to min and strictly below max,
where (in little endian order) the digits of min are (1+ LH) zeros followed by
then the digits of Q, and the digits of max are LH zeros then D then the digits
of Q. H is (SUMS LH). FF is (EXPT 10 LH).
LQ is the length of Q, Q7 is (%7 Q), R7 is (%7 (reverse-number Q))."
  ;(dbg PS0 lh d q lq q7 r7 ff)
  ;(dbg PS1 h)
  (loop
     with r0 = 0
     with r1 = 0
     with f = (expt%7 10 (- lh))
     with g = (expt%7 10 lq)
     for i below 7
     for k = (%7 (- 0 (*%7 q7 f) i))
     for l = (%7 (- 0 r7 (*%7 i g))) do
     ;(dbg PS2 i k l)
     (multiple-value-bind (c0 c1)
	 (multiple-value-bind (a0 a1 b0 b1)
	     (values
	      (aref *sum-to* (1- d) i 0) (aref *sum-to* (1- d) i 1)
	      (aref h k l 0) (aref h k l 1))
	   ;(dbg PSL r0 r1 a0 a1 b0 b1 f g k l)
	   (values (* a0 b0) (+ (* a1 b0) (* a0 b1 ff))))
       (incf r0 c0)
       (incf r1 c1))
     finally (return (+ r1 (* r0 ff 10 q)))))

#|
[0..23400567)
=
[00000000..19999999] +
[20000000..22999999] +
[23000000..23399999] +
[23400000..23400499] +
[23400500..23400559] +
[23400560..23400566]
|#

(defun clever-count (n)
  "efficiently count the number of lucky sevens up to some arbitrary given number N"
  ;(dbg clever-count n)
  (loop
     with q = (1+ n)
     with lq = (number-length q) ; length of q
     with d = 0
     with p = 0
     with q7 = (%7 q)
     with r7 = (%7 (reverse-number q))
     with f = 1
     with g = (expt%7 10 (1- lq))
     with sp = *sums0*
     with h = *sums0*
     with lh = 0
     with ff = 1
     with r = 0
     while (> lq 0) do
     ;(dbg CCL0 q lq d p q7 r7 f g lh ff r)
     ;(dbg CCL1 sp h)
     (loop while
	 (progn
	   (multiple-value-setq (q d) (floor q 10))
	   (setf lq (1- lq)
		 q7 (-%7 q7 (* f d))
		 r7 (-%7 r7 (* g d))
		 f  (*%7 f  +10%7+)
		 g  (*%7 g  +1/10%7+))
	   (zerop d))
       do (incf p))
     ;(dbg CCL2 p lq q7 r7 f g q d)
     (setf sp (sums p)
	   h (if (zerop lh) sp (combine-sums h sp lh p))
	   lh (+ lh p)
	   r (+ r (partial-sum h lh d q lq q7 r7 ff))
	   ff (* ff (expt 10 p))
	   p 1)
     ;(dbg CCL3 lh ff r p)
     ;(dbg CCL4 sp h)
     finally (return r)))

(defun re () (load "/home/fare/private/puzzles/ita-seven.lisp"))

(defun print-solution (&optional (solution (clever-count *max*)))
  (format t "~%The solution is ~A~%" solution))

(defun solve-problem (&optional (n *max*))
  (time (print-solution (clever-count n)))
#| (format t "~%But if you're rather interested in numbers below 10^11111, then~%")
   (time (print-solution (clever-sum 11111))) |#
)

#|
END-OF-LISP
Solving a simple problem from ITA

#!/bin/sh

### This script launches the SOLVE-PROBLEM function with clisp or whichever
### lisp implementation it finds (supported: cmucl, sbcl, clisp).

## You may specify your LISP implementation in environment variable LISP,
## or by editing this file to define yours (if not yet defined) and/or
## give it higher priority.

DIR=$(dirname $0)
PROG=$(basename $0 .lisp)

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
  OPTIONS="-norc --quiet"
  LOAD=-i
  EVAL=-x
  FASL=fas
}

trylisp () {
  if LISPBIN=`which $1` ; then set_$1 ; return 0 ; else return 1 ; fi
}
{ [ -n "$LISP" ] && trylisp $LISP ; } ||
trylisp clisp ||
trylisp cmucl ||
trylisp lisp ||
trylisp sbcl ||
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

if [ $# -gt 0 ] ; then
  NUMBER="$1"
else
  NUMBER="100000000000"
fi

## Now at last, invoke lisp with our compiled program...
$LISP $OPTIONS $EVAL "(progn (load \"$L/$PROG.$FASL\") (solve-problem $NUMBER) (quit))"

#\/ |#
