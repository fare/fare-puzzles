#!/bin/sh
":" ; exec cl-launch -X -- "$0" "$@"
;(load "/usr/share/common-lisp/source/cl-launch/header.lisp")

;;; This program is NOT a complete (yet).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :fare-utils)
  (defpackage :pp (:use :cl :fare-utils)))
(in-package :pp)


;;;;; User-configurable settings ;;;; <<<<<<<<<<< EDIT ME !
(defparameter *dictionary-file* (merge-pathnames #p"puzzles/WORD.LST" (user-homedir-pathname)))
;; cat puzzles/WORD.LST | grep -v '[^ban]' > puzzles/w

;;; End of settings. No serviceable part below.


;;;;; Setup
(eval-when (:compile-toplevel :load-toplevel :execute)


;(proclaim '(optimize (speed 3) (space 0) (safety 0) (compilation-speed 0)))
(proclaim '(optimize (speed 3) (space 0) (safety 1) (debug 3) (compilation-speed 0)))

(pushnew :verbose *features*)
;;(pushnew :paranoid *features*)
)

;;;;; Utilities
(defmacro maybe-time (comment &body body)
  #+verbose `(progn (format t "Timing: ~A...~%" ,comment) (time (progn ,@body)))
  #-verbose `(progn ,@body))

(defmacro defbiniop (name orig)
  `(defmacro ,name (x y)
    `(locally (declare (optimize (speed 3) (safety 0)))
      (the fixnum (,',orig (the fixnum ,x) (the fixnum ,y))))))
(defmacro defcmpiop (name orig)
  `(defmacro ,name (x y)
    `(locally (declare (optimize (speed 3) (safety 0)))
      (the boolean (,',orig (the fixnum ,x) (the fixnum ,y))))))
(defbiniop i+ +)
(defbiniop i- -)
(defcmpiop i= -)
(defcmpiop i< <)
(defcmpiop i> >)
(defcmpiop i<= <=)
(defcmpiop i>= >=)
(defbiniop iash ash)
(defbiniop iior logior)
(defbiniop ixor logxor)
(defbiniop iand logand)
(defbiniop iandc2 logandc2)
(defsubst i1- (x &optional (y 1)) (i- x y))
(defsubst i1+ (x &optional (y 1)) (i+ x y))

(define-modify-macro iincf (&optional (x 1)) i1+)
(define-modify-macro idecf (&optional (x 1)) i1-)
(define-modify-macro iiorf (x) iior)
(define-modify-macro ixorf (x) ixor)

(defsubst zp (x) (eql 0 x))

;;;;; Interning the dictionary
(defun read-dictionary ()
  (with-open-file (dict *dictionary-file* :direction :input)
    (loop for i = (read-line dict nil nil nil)
          while i collect (string-downcase i))))

(defun reverse-dictionary (dictionary)
   (sort (map 'vector 'reverse dictionary) #'string<))

(defvar *dictionary* nil)


;;;;; Efficient implementation of letter sets with fixnum bitmasks
(defparameter +letter-mask+
  (make-array 26 :element-type 'fixnum :initial-contents
	      (loop for m fixnum = 1 then (i+ m m) repeat 26 collect m)))
(defun index-mask (li)
  (declare (type fixnum li)
	   (type (simple-array fixnum (26)) +letter-mask+))
  (if li (aref +letter-mask+ li) 0))
(defsubst mask-index (m)
  (declare (type fixnum m))
  (loop
    for lm fixnum = 1 then (i+ lm lm)
    for i from 0 below 26
    unless (zp (iand m lm)) do (return i)
    finally (return nil)))
(defun index-letter (li)
  (and li (code-char (i+ li (char-code #\a)))))
(defun mask-letter (m) (index-letter (mask-index m)))
(defun letter-index (l)
  (declare (type character l))
  (let ((i (i- (char-code l) (char-code #\a))))
    (when (<= 0 i 25) i)))
(defun letter-mask (l)
  (index-mask (letter-index l)))
(defun word-mask (w)
  (loop with m = 0
     for l across w do
     (setf m (iior m (letter-mask l)))
     finally (return m)))
(defun mask-word (mask)
  (coerce
    (loop for i from 0 below 26
          for l = (index-letter i)
          for m = (index-mask i)
          when (mask-intersect-p m mask)
          collect l)
    'string))
(defsubst mask-weight (m)
  ;; number of letters in a mask, in 15 ALU operations.
  ;; A simple iteration over letters is two ALU operations per letter,
  ;; plus memory fetch, plus conditional, and so loses starting with 4 letters or so.
  (declare (type (unsigned-byte 26) m)) ; #x3FFFFFF
  (macrolet ((f (x s m) `(i+ (iand (iash ,x ,s) ,m) (iand ,x ,m))))
  (let* ((m1 (f m -1 #x1555555))
	 (m2 (f m1 -2 #x333333))
	 (m3 (f m2 -4 #x70707))
         (m4 (f m3 -8 #xf000f))
         (m5 (f m4 -16 #x1f)))
    m5)))
(defsubst full-mask-below (m) (i1- m))
(defsubst full-mask-to (m) (i1- (iash m 1)))
(defsubst mask-intersect-p (m1 m2) (not (zp (iand m1 m2))))

;;;;; checks on input
(defun check-dictionary (dictionary)
  (let ((warn-length nil)
        (warn-glyph nil))
    (setf dictionary
          (coerce
           (delete-if
            #'(lambda (w)
                (or
                 (unless (>= 29 (length w)) (setf warn-length t))
                 (unless (every #'letter-index w) (setf warn-glyph t))))
            dictionary)
           'vector))
    #+verbose
    (when warn-length
      (format t "Dropping words over 29 letters long~%"))
    #+verbose
    (when warn-glyph
      (format t "Dropping words with character other than ascii letters~%"))
    (unless
        (loop for (x y . r) on dictionary
              always (string< x y))
      #+verbose
      (format t "dictionary not sorted. Sorting.~%")
      (setf dictionary (sort dictionary #'string<)))
    #+verbose
    (format t "Dictionary length: ~A~%" (length dictionary))
    dictionary))


;;;;; Building a big but efficient representation of the grammar for palindromes
#|
Take the list of words.
Sort it in a ways that makes it easy to find prefixes and suffixes.
For each word, compute the word, its letter-mask,
and articulation of the palindromic network from there, as follows.

For each word, compute the set of valid (prefix,suffix) pairs of its *reverse*, where
(1) prefix is the suffix of (at least) another word, and
(2) the letters between prefix and suffix can be made of words, and
(3a) suffix is a palindrome, or
(3b) suffix is the prefix of (at least) another word.
[Note that 3b is same as 1 with the two words reversed.]
Make it easy to find the set of valid prefixes from a suffix
and valid suffixes from a prefix.
Make it easy to find from a given prefix or suffix string
the words that have this string as prefix or suffix,
and the words the reverse of which may overlay it.

Trimming the grammar representation:
We can unlink from a word's reverse
the prefixes to which no suffix is associated,
and the suffixes that have no associated prefixes.
We can unlink from a suffix or prefix string
the words for which said prefix is not valid.
We can invalidate suffixes or prefixes that are found
to have no linked words, and unlink them from the words
that overlay into them.
(unlink first, check for being last link after, to not cycle indefinitely?).
(Finally, we can compact all that in a table without holes?)

[We could do it the other way round:
palindromic suffixes are the starting point,
and we find a least fix-point from there.
We eliminate suffixes that do not contain revtree in their closure.
However, wouldn't the fix-point require quite a lot of computation?
If it's in O(n^2), and the dictionary has 2e5 entries, that's too much
(and lots of memory, too). If it's O(n), on the other hand we win big.
It looks like it's O(n), because we can propagate "life" from (), and
recurse at most one per node and edge. It's garbage collection.
Two phases: (1) walk all the nodes from ().
(2) kill those that didn't reach a palindrome
There is a finite state machine for recognizing palindromes;
it's a regular language.
Can we find it all (and virtually generate all palindromes) in doable time?
Number of states: number of nodes by number of father nodes, i.e. N=O(n*d^2).
Find all paths from trie to word: P=N*d=O(n*d^3).

remember for each fix:
* the or mask of letters of the paths that lead there (no, that would require a fix point in O(P^2))
* the min of the length of path to there. (or just bit)
* whether it's know to lead to a palindrome O(P)
at the end, if not all and finite and yes, GC

walk:
* for each word and each decomposition in two reachable fix/co-fix O(n*d^2),
 link one fix to the reverse of the co-fix.
* There's O(n^2*d^2) possible maximum pairs of fix/co-fix (fewer in practice?)
 Count them before doing anything silly...
* Then from each live and previously unprocessed fix, starting from top and bottom,
 walk all the pairs of which it is part, and mark the other end as live.
* May take quite some time, and a lot of memory(!)

And more so, having hints to find the shortest pangramatic solution?
It's easy to remember the shortest solution between one point and ()
(do a breadth-first traversal).
but requires too much state to remember the shortest solution b/w all pairs of points


Map fixes to shorter fixes => OK.
Map words to a lattice of pairs of fixes => ?
A word is just an end point in such a lattice:
it is a fix coupled with the top co-fix.
Map fix to reverse fix (and thus the words that begin with it).
Once I have a fix + a word under that fix, how can I efficiently find valid associated co-fixes?
Not all words under a fix are valid under said fix.
For every word, have a bitmap of fixes of that word
(Do we have a use for mapping every word as fix to same word in co-fix?)

Start from one end, find reachable palindromic 


Note that the empty string is both a valid prefix and suffix for all words
(and initially, their reverse, until removed).

FIREMAN => (BANA)NA-ME-RIF(FLE), and so we'll have the associated pairs AN and RIF.

()... ()
BA ...AB
BA BAN... NABAB
BA BANANA NABAB
BA BANCO ...OC NABAB

Include among words the empty word, that is a valid suffix, reverse suffix
and prefix of all words (?)
The list of all suffixes, etc., are but the appropriate fiels of that word(!)

then recursively clear prefixes that provably have no valid attached suffixes
(kind of a reference-listing garbage collection).

for every word and every prefix, fit reverse words in what remains (n*maxl*maxl)
until the rest is a valid reverse prefix, remember the list of possible continuations
also remember if the rest is palindromic
remove prefixes that have no valid suffix


search strictly under a maximum excess weight of 1, 2, 4, 8, 16...
(stop doubling when solution is found -- use previous best as limit)

start from empty phrase
if the current weight exceeds the maximum fail.
if the sought letter set is empty, then just look for palindrome:
  if the dangling prefix is palindromic, we're done! -- print
  otherwise, sort continuations by weight, explore them.
if the sought letter set is not empty, sort possible continuations
 by what they bring: reduction in sought letter set, excess weight.

WORDS.LST: 173528 mots, 9 lettres en moyenne

Possible heuristics:

Instead of starting from the end [] and trying to converge toward the middle,
we can do it the other way and do more of a breadth-first traversal.
Then we always generate new anagrams, and if we change letters,
we can combine them (at high cost) into something, yielding non-optimal solutions.

Depth first means that any time you make a bad move, you have to go all the way.
And since there is no extra short cut, that sucks

We could queue candidates and run the more likely to win, in a lossy breadth-first traversal.
If the shortest has length 57, this means that we have an average
of 8.5 useful characters per useless one.

We can filter the dictionary to words that are overly bad, especially if they are long.
We can probably be more lenient on redundant characters if they are frequent characters,
and come bundled with rare characters.

We cannot compute the fixpoint of the matrix of all fixes,
but we can start from the best combinations, and double the size everytime,
keep the number of candidates in check.
In any case, it might be good if we start from anywhere.
Or if we find...

Better idea for a way that preserves comprehensiveness of the proof:
* start from _both_ the middle and the edge, cutting the complexity in two.
 Then we only have to exhaustively generate promiseful strings
 (without too many excess characters) of less than 15 characters,
 instead of strings of length 29 (or 57), a dastric square rooting
 of the combinatorics!
* The actual length of strings to generate on either side depends a bit:
 if some strings on one side are very good, this leaves leeway for a lot
 of excess (and thus combinatoric explosion) on this other side;
 the solution is to instead prolong the few strings on the good size
 in the few ways that they can be prolonged, which is the most constrained
 set, thereby reducing the required length of the wild side,
 which exponentially reduces its complexity.
* We thus have a game where at each turn, we will grow wichever end is smaller.

I tried to generate half solutions. But there are still too many of them.
we should try pre-generating thirds, and then completing matching pairs
through a set of very constrained continuations. i.e. breadth first
as much as can be reasonably fit into memory, then

|#

(defstruct (fix (:conc-name fix-))
  ;;; These define a standard trie
  (up nil :type (or fix null)) ;; previous fix
  (char 0 :type fixnum) ;; what is the char that defines this fix?
  (continuations #() :type simple-vector) ;; a sorted vector of fix'es
  ;;; Remember depth
  ;(depth 0 :type fixnum) ;; length of the string so far
  (depthmask 0 :type fixnum) ;; (iash 1 depth)
  ;;; Our trie is a dictionary
  (flags 0 :type fixnum) ;; is the fix a complete word?
  ;;(charmask 0 :type fixnum) ;; what are the chars in this fix? mask of chars in the fix, plus a bit for wordp? for active?
  ;;; These are related to palindromes
  (linkmask 0 :type fixnum) ;; which ancestors can be reached directly from this fix?
  (stemmask 0 :type fixnum) ;; which ancestors can be reached from this fix?
  (substemmask 0 :type fixnum) ;; which ancestors can be reached from below this fix?
  (stopmask 0 :type fixnum) ;; for which ancestors is this fix a stopper?
  (substopmask 0 :type fixnum) ;; for which ancestors are there stoppers below?
  (cofix nil :type (or fix null)) ;; cofix, if available. i.e. ...ABC => ABC... -- if word, same from the other view
; (corev nil :type (or fix null))) ;; coreverse, if available. i.e. ...ABC => CBA... -- if word, reverse from the other view
  (reverse nil :type (or fix null))) ;; reverse fix, if available. i.e. ...ABC => ...CBA -- if word, reverse from same view

(defmacro def-flag-reader (name mask &optional xormask)
  `(defsubst ,name (fix)
    (declare (type fix fix))
    (mask-intersect-p
     ,(let ((flags `(the fixnum (fix-flags fix))))
           (if xormask `(ixor ,flags ,xormask) flags))
     ,mask)))
(defmacro def-flag-writer (name mask)
  `(defsubst (setf ,name) (value fix)
    (declare (type fix fix))
    (if value
      (iiorf (fix-flags fix) ,mask)
      (iandc2 (fix-flags fix) ,mask))
    value))
(defmacro def-flag (name mask)
  `(progn
    (def-flag-reader ,name ,mask)
    (def-flag-writer ,name ,mask)))

(def-flag fix-wordp 1)
(def-flag fix-compoundp 2)
(def-flag-reader fix-word-or-compound-p 3)
(def-flag fix-stopp 4)
(def-flag fix-palhp 8)
(def-flag fix-subpalhp 16)
(def-flag fix-palhop 32)
;;(def-flag fix-livep 32)

(defsubst fix-livep (fix)
  (or (fix-wordp fix) (not (empty-p (fix-continuations fix)))))

(defun empty-p (x) (zp (length x)))

(defun fix-stemmask* (fix)
  (iior (fix-stemmask fix) (fix-substemmask fix)))
(defun fix-stopmask* (fix)
  (iior (fix-stopmask fix) (fix-substopmask fix)))

(defun wchars (w) (map 'list 'letter-mask w))
(defun charsw (l) (map 'string 'mask-letter l))
(defun fixw (f) (charsw (fix-chars f)))
(defun cofixw (fix) (charsw (fix-back-chars fix)))
(defun fixconts (f) (charsw (map 'vector 'fix-char (fix-continuations f))))
(defsubst fix-rootp (fix) (zp (fix-char fix)))
(defun fix-root (fix)
  (loop for f = fix then up for up = (fix-up f)
        while up finally (return f)))

(defun fix-flag-desc (f)
  (concatenate
   'string
   (when (fix-wordp f) "W")
   (when (fix-compoundp f) "C")
   (when (fix-stopp f) "S")))
(defun char-decor (c)
  (if (alpha-char-p c) (char-upcase c)
    (let ((p (position c "<>?")))
      (if p (aref "{}!" p) c))))
(defun wdecor (w flags)
  (loop for i below (length w)
        for mask = 1 then (iash mask 1)
        when (mask-intersect-p mask flags)
        do (setf (aref w i) (char-decor (aref w i))))
  w)

(defvar *prefix-trie* nil)
(defvar *suffix-trie* nil)

(defvar *fix-decor* (constantly 0))

(defun fix-pre? (fix) (eq (fix-root fix) *prefix-trie*))
(defun fix-suf? (fix) (eq (fix-root fix) *suffix-trie*))

(defun fixx (fix &optional (decor *fix-decor*))
  (let* ((w (fixw fix))
         (pre? (fix-pre? fix))
         (suf? (fix-suf? fix))
         (x (cond (pre? ">")
                  (suf? "<")
                  (t "?")))
         (s (format nil "~A~A" w x))
         (d (wdecor s (funcall decor fix)))
         (r (if suf?
              (if pre?
                (format nil "(~A" d)
                (reverse d))
              d)))
    r))

(defmethod print-object ((f fix) stream)
  (if (and (fix-suf? f) (not (fix-pre? f)))
    (format stream " ~A/~A/~A "
            (fixconts f)
            (fixx f)
            (fix-flag-desc f))
    (format stream " ~A/~A/~A "
            (fixx f)
            (fixconts f)
            (fix-flag-desc f))))

(defun do-collect-fix (n start end dictionary mechar up)
  (loop
    with terminalp = (= n (length (svref dictionary start)))
    with first = (if terminalp (i1+ start) start)
    with current = nil
    with continuation-chars = nil
    with continuation-starts = nil
    for index from first below end
    for word = (aref dictionary index)
    for char = (char word n)
    unless (eql char current) do
    (setf current char)
    (push char continuation-chars)
    (push index continuation-starts)
    finally (let* ((n+1 (i1+ n))
		   (fix (make-fix :up up
				  :char (if mechar (letter-mask mechar) 0)
				  ;;:depth n
                                  :depthmask (iash 1 n)
				  :flags (if terminalp 1 0)))
		   (continuation-ends (cons end continuation-starts))
		   (continuations
		    (mapcar #'(lambda (char start end)
				(do-collect-fix n+1 start end dictionary char fix))
			    continuation-chars continuation-starts continuation-ends)))
	      (setf (fix-continuations fix) (apply 'vector (nreverse continuations)))
	      (return fix))))
(defun collect-fix-trie (dictionary)
  "make a fix trie from a dictionary"
  (do-collect-fix 0 0 (length dictionary) dictionary nil nil))
(defun reverse-trie (trie)
  (collect-fix-trie (reverse-dictionary (list-fix-words trie))))

(defun find-sorted (elem set pred &key key (test #'eql))
  (labels ((try (min max)
		(when (< min max)
		  (let* ((mid (floor (iash (i+ min max) -1)))
			 (x (aref set mid))
			 (k (funcall key x)))
		    (cond
		     ((funcall pred elem k) (try min mid))
		     ((funcall pred k elem) (try (i1+ mid) max))
		     ((funcall test k elem) x)
		     (t nil))))))
    (try 0 (length set))))

(defun step-fix (char fix)
  (find-sorted char (fix-continuations fix) #'< :key 'fix-char))
(defun fix-lookup (chars fix)
  (cond
   ((null chars) fix)
   ((or (null fix) (empty-p (fix-continuations fix))) nil)
   (t (fix-lookup (rest chars) (step-fix (first chars) fix)))))
(defun fix-find (chars fix)
  (let ((f (fix-lookup chars fix)))
    (and f (fix-wordp f) f)))

(defun prefix (word)
  (fix-lookup (wchars word) *prefix-trie*))
(defun suffix (word)
  (fix-lookup (reverse (wchars word)) *suffix-trie*))
(defun p (&optional (word "")) (prefix word))
(defun s (&optional (word "")) (suffix word))

(defun reduce-fix (fix stepper &optional default-step (reducer (constantly nil)) default-reduction)
  (funcall stepper fix
           #'(lambda (fix)
               (reduce reducer (fix-continuations fix) :initial-value default-reduction))))

(defmacro do-with-fix ((fixvar &optional (fixtree fixvar) block)
                       &key body before around within after)
  (with-gensyms (tmp recurse)
  `(let ((,tmp ,fixtree))
      (labels ((,recurse (,fixvar)
                   (block ,block
                     (macrolet ((recurse ()
                                  '(progn
                                    ,before
                                    ,body
                                    (let ((,tmp (fix-continuations ,fixvar)))
                                      (loop for ,fixvar across ,tmp
                                            do
                                            (macrolet ((recurse () '(,recurse ,fixvar)))
                                              ,(or within '(recurse)))))
                                    ,@(when after (list after)))))
                       ,(or around '(recurse))))))
      (,recurse ,tmp)))))

(defmacro do-with-fix-ancestors (((ancestor &optional (fix ancestor))
                                  &key stem (char (gensym)))
                                 &key body loopbody when-completes)
  (destructuring-bind (&optional word stemmer &key reverse) stem
    (with-gensyms (f cell chars)
      `(loop
        ,@(when word
                `(with ,word = ,(unless reverse stemmer)
                  ,@(when reverse `(with ,char fixnum = (fix-char ,fix)
                                    with ,cell = (list nil ,char)
                                    with ,chars = (cdr ,cell)))))
        for ,f = ,fix then ,ancestor
        for ,ancestor = (fix-up ,f)
        ,@(unless (and word reverse) `(for ,char fixnum = (fix-char ,f)))
        until (zp ,char) do
        ,@(when word
                `(,(if reverse
                     `(progn
                       (setf ,cell (cdr ,cell))
                       (setf ,word (fix-lookup ,chars ,stemmer)))
                     `(progn
                       (setf ,word (step-fix ,char ,word))
                       (unless ,word (return))))))
        (progn ,body)
        ,@loopbody
        ,@(when (and word reverse)
                `(do (setf ,char (fix-char ,ancestor) (cdr ,cell) (list ,char))))
        ,@(when when-completes `(finally (return ,when-completes)))))))

(defun fix-back-chars (fix)
  (do-with-fix-ancestors ((f fix) :char char)
    :loopbody (collect char)))
(defun fix-chars (fix)
  (let (chars)
    (do-with-fix-ancestors ((f fix) :char char)
      :body (push char chars))
    chars))
(defun reverse-fix (fix tree)
  (fix-find (fix-back-chars fix) tree))

(defun fix-size (fix)
  (let ((nodes 0)
        (words 0))
    (do-with-fix (fix)
      :body (progn (incf nodes) (when (fix-wordp fix) (incf words))))
    (values words nodes)))

(defun fix-count (fix pred)
  (let ((nodes 0))
    (do-with-fix (fix)
      :body (when (funcall pred fix) (incf nodes)))
    nodes))

(defun list-fix-words (fix) ;; lists the words below a fix, in reverse order
  (let (list)
    (do-with-fix (fix)
      :body (when (fix-wordp fix) (push (fixw fix) list)))
    list))

(defun print-fix-words (fix &optional (stream t))
  (do-with-fix (f fix)
    :body (when (fix-wordp f) (format stream "~A~%" (fixw f)))))

(defun fix-palindromic-p (fix)
  (eq fix (fix-reverse fix)))

(defun palindromic-p (seq)
  (loop with vec = (coerce seq 'vector)
    for i from 0
    for ri downfrom (1- (length vec))
    while (< i ri)
    always (eql (aref vec i) (aref vec ri))))

(defun print-fix-palindromic-successors (fix &optional (stream t))
  (do-with-fix (f fix)
     :body (when (fix-palindromic-p f) (format stream "~A~%" (fixw f)))))

(defmacro stem-trie (((fix ancestor &optional (word (gensym))) trie stemtrie &key reverse word-only)
                     &key setup add (guard t) complete cleanup)
  ;; remove words that are decomposable: they only make the problem worse
  ;; (hey, that's the fireman problem!)
  `(do-with-fix (,fix ,trie)
    :before ,setup
    :body (when ,guard
            (do-with-fix-ancestors ((,ancestor ,fix) :stem (,word ,stemtrie :reverse ,reverse))
              :body
              (when ,(if reverse `(and ,word (fix-wordp ,word)) `(fix-wordp ,word)) ,add)
              :when-completes ,complete))
    :after ,cleanup))

(defun trim-trie (trie revtrie)
  ;; remove words that are decomposable: they only make the problem worse
  ;; (hey, that's the fireman problem!)
  (stem-trie ((fix ancestor cofix) trie revtrie)
   :setup
   nil ;; (setf (fix-compound-p fix) nil)
   :add
   (when (fix-word-or-compound-p ancestor)
     ;;(setf (fix-wordp fix) nil (fix-compoundp fix) t) ; buggy compiler???
     (setf (fix-flags fix) 2))
   :complete
   (setf (fix-cofix fix) cofix)
   :cleanup
   (setf (fix-continuations fix)
         (delete-if-not #'fix-livep (fix-continuations fix)))))

(defun register-half-palindromic-fix (fix trie)
  (loop
    for downward = trie then (step-fix (fix-char upward) downward)
    for upward = fix then next-upward
    for next-upward = (fix-up upward)
    until (or (eq downward upward) (eq downward (fix-up upward)))
    do (setf (fix-subpalhp downward) t)
    finally (setf (fix-palhp upward) t (fix-palhop upward) (eq downward upward))))

(defun palindromic-fix-from-half (fix)
  (loop
    for downward = fix then (step-fix (fix-char upward) downward)
    for upward = (if (fix-palhop fix) fix (fix-up fix)) then (fix-up upward)
    until (fix-rootp upward)
    finally (return downward)))

(defun find-back-stems (trie)
  (stem-trie
   ((fix ancestor reverse) trie trie)
   :setup
   (setf (fix-stemmask fix) (fix-depthmask fix)
         (fix-linkmask fix) 0)
   :add
   (progn
     (iiorf (fix-stemmask fix) (fix-stemmask ancestor))
     (iiorf (fix-linkmask fix) (fix-depthmask ancestor)))
   :complete
   (progn
     (setf (fix-reverse fix) reverse)
     (when (eq fix reverse)
       (register-half-palindromic-fix fix trie)))
   :cleanup
   (setf (fix-substemmask fix)
         (iand
          (full-mask-to (fix-depthmask fix))
          (reduce #'logior (fix-continuations fix)
                  :key #'fix-stemmask* :initial-value 0)))))

(defun find-stops (trie cotrie)
  ;; find which words complete which stop fixes.
  (stem-trie
   ((fix ancestor coreverse) trie cotrie :reverse t)
   :setup
   (setf (fix-stopp fix) nil
         (fix-stopmask fix) (if (fix-wordp fix) (fix-depthmask fix) 0)
         (fix-substopmask fix) 0)
   :guard
   (when (fix-wordp fix)
     (setf (fix-stopp fix) t))
   :add
   (progn ;; if we reached here, then the cofix is reversible!
     (setf (fix-stopp ancestor) t)
     (iiorf (fix-stopmask fix) (fix-stemmask ancestor)))
   :complete
   nil ;; (setf (fix-corev fix) coreverse)
   :cleanup
   (setf (fix-substopmask fix)
         (iand
          (full-mask-to (fix-depthmask fix))
          (reduce #'logior (fix-continuations fix)
                  :key #'fix-stopmask* :initial-value 0)))))

;------>8------>8------>8------>8------>8------>8------>8------>8------>8------

;;;;; Now for search heuristics

(defvar *attempts* 0 "number of attempts so far")
(defvar *length* 0 "length of the string so far")
(defparameter *attempts-mask* #x1FFFF)
(defvar *solution* nil "the best solution we have found so far")
(defvar *sought-mask* 0 "mask of remaining letters to fulfill")
(defvar *sought-length* 0 "length of remaining letters to fulfill")
(defvar *target-length* 0 "length of targets to generate")
(defvar *allowed-excess* 2 "maximum remaining acceptable cost, starting from 1 for theoretical limit")
(defvar *even-excess-p* t "whether it is considered an excess that the middle letter shall be doubled")
(defvar *cost* 0 "cost of the string so far")
(defvar *fix-cost* 0 "cost of the fix being scanned (- length foundletters)")
(defvar *fix-length* 0 "length of the fix being scanned so far")
;(defvar *max-interest* 1e6 "maximal interest for a word to consider.")
(defvar *pre* t "are we considering prefixes?")
(defvar *fix* *prefix-trie* "the current fix")
(defvar *cofix* *suffix-trie* "the current cofix")
(defvar *options* nil "next things to try")
(defvar *path* nil "list of fixes")
(defvar *trie* *prefix-trie* "the trie of all fixes")
(defvar *cotrie* *suffix-trie* "the trie of all cofixes")
(defvar *count* 0 "counter")
(defvar *word* nil "counter")
(defvar *stop* nil "counter")

(declare (type fixnum
               *sought-mask* *allowed-excess* *cost* *fix-cost*
               *attempts* *attempts-mask* *sought-length*))
(defstruct (option (:conc-name option-))
  (cost nil :type fixnum)
  (word nil :type (or fix null))
  (stop nil :type (or fix null))
  (cofix nil :type (or fix null))
  (sought-mask 0 :type fixnum)
  (length 0 :type fixnum))

(defmethod print-object ((o option) stream)
  (princ
   `(option (:cost ,(option-cost o))
     (:seek ,(mask-word (option-sought-mask o))) (:length ,(option-length o))
     (:word ,(option-word o)) (:stop ,(option-stop o)) (:cofix ,(option-cofix o)))
   stream))

(defun init-search ()
  (setf *attempts* 0
        *solution* nil
        *allowed-excess* 2
        *sought-mask* (word-mask "abcdefghijklmnopqrstuvwxyz")
        *sought-length* (mask-weight *sought-mask*)
        *target-length* (ash *sought-length* 1)
        *cost* 0
        *fix-cost* 0
        *pre* t
        *fix* *prefix-trie*
        *path* nil
        *trie* *prefix-trie*
        *cotrie* *suffix-trie*)
  nil)

(defmacro do-with-char ((char) &body body)
  (evaluating-once (char)
  `(let*
    ((*sought-mask* *sought-mask*)
     (*sought-length* *sought-length*)
     (*cost* *cost*)
     (*length* (i1+ *length*)))
    (if (mask-intersect-p *sought-mask* ,char)
      (progn (ixorf *sought-mask* ,char) (idecf *sought-length*))
      (iincf *cost*))
    ,@body)))

(defun stoplen (x) (loop for l across x sum (length l)))

(defun register-solution (cost word path)
  (setf *solution* (list cost word path)
        *allowed-excess* cost)
  (format t "Woohoo! Found a solution of cost ~A: " cost)
  (display-solution *solution*))

(defun display-fixes (fixes)
  (loop for fix in (reverse fixes)
        for first = t then nil do
        (unless first (write-char #\space))
        (write-string (fixw fix))))
(defun display-cofixes (cofixes)
  (loop for cofix in cofixes for first = t then nil do
        (unless first (write-char #\space))
        (write-string (cofixw cofix))))

(defun find-stem-path (fix stop trie)
  (unless (eq (fix-root stop) (fix-root fix))
    (error "I was asked to find a stem path between fixes of different tries"))
  (unless (mask-intersect-p (fix-stemmask stop) (fix-depthmask fix))
    (error "I was asked to find a stem path to a fix that is not reachable"))
  (unless (eq fix stop)
    (do-with-fix-ancestors
        ((ancestor stop) :stem (word trie))
      :body
      (progn
        #+nil
        (format t "~A ~A ~16R ~16R ~A ~16R ~16R ~A~%"
                ancestor word
                (fix-linkmask stop) (fix-depthmask ancestor)
                (mask-intersect-p (fix-linkmask stop) (fix-depthmask ancestor))
                (fix-stemmask ancestor) (fix-depthmask fix)
                (mask-intersect-p (fix-stemmask ancestor) (fix-depthmask fix)))
        (when (and (mask-intersect-p (fix-linkmask stop) (fix-depthmask ancestor))
                   (mask-intersect-p (fix-stemmask ancestor) (fix-depthmask fix)))
          (return-from find-stem-path
            (cons (fix-cofix word) (find-stem-path fix ancestor trie))))))))

(defun display-path (path final-fix endp)
  (loop with fixes = () with cofixes = ()
        with trie = *prefix-trie* with cotrie = *suffix-trie*
        with fix = *prefix-trie*
        for pre = t then (not pre)
        for option in (reverse path) do
        (push (option-word option) fixes)
        (setf cofixes (nconc (find-stem-path fix (option-stop option) trie)
                             cofixes))
        (psetf fixes cofixes
               cofixes fixes
               trie cotrie
               cotrie trie
               fix (option-cofix option))
        finally
        (multiple-value-bind (fixes cofixes wrap)
            (if pre
              (values fixes cofixes 'identity)
              (values cofixes fixes 'reverse))
          (display-fixes fixes)
          (write-string
           (funcall wrap (format nil " ~A~A  "
                                 (if final-fix (fixw final-fix) "")
                                 (if endp "" "..."))))
          (display-cofixes cofixes))))

(defun display-solution (solution &optional (*standard-output* *standard-output*))
  (destructuring-bind (cost word path) solution
    (declare (ignore cost))
    (display-path path word t)
    (terpri)))

(defun display-state ()
  (format t "seek ~A  ~A/~A  [" (mask-word *sought-mask*) *cost* *allowed-excess*)
  (display-path *path* *fix* nil)
  (format t "]~%"))

(defun random-option ()
  (when *options*
    (nth (random (length *options*)) *options*)))

(defun number-options ()
  (find-all-options)
  (loop for i from 0 for o in *options* do
        (format t "~A ~A~%" i o))
  (display-state)
  nil)
(defun choose-option (n)
  (enact-option (nth n *options*))
  (display-state))
(defun nav ()
  (macrolet ((x (s &body body) `(when (eql x ,s) ,@body (again))))
    (labels
        ((again ()
           (display-state)
           (number-options)
           (format t "NAV> ")
           (let ((x (read)))
             (x 'b (pop-state))
             (x 'r (simpler))
             (x 'x (return-from nav))
             (cond ((and (integerp x) (< -1 x (length *options*)))
                    (push-state) (choose-option x) (again))
                   (t (print (eval x)) (terpri) (again))))))
      (again))))

(defun save-current-option ()
  (make-option
   :cost *cost* :sought-mask *sought-mask* :length *length*
   :word *word* :stop *stop* :cofix *cofix*))

(defun walk-words-after-stop (word-processor)
  (declare (optimize (speed 1) (safety 3) (debug 3)))
  (let ((*cofix* *cotrie*)
        (stopmask (fix-depthmask *stop*)))
    (do-with-fix (*word* *stop*)
      :within
      (let* ((char (fix-char *word*))
             (*cofix* (step-fix char *cofix*)))
        (unless *cofix* (return))
        (do-with-char (char) (recurse)))
      :body
      (progn
        (unless (i< *cost* *allowed-excess*) (return))
        (when (and (fix-wordp *word*)
                   (mask-intersect-p stopmask (fix-stopmask *word*)))
	  (funcall word-processor))
        (unless (mask-intersect-p stopmask (fix-substopmask *stop*)) (return))))))

(defun walk-all-stops (stop-processor)
  (let ((stubmask (fix-depthmask *fix*)))
    (do-with-fix (*stop* *fix*)
      :within
      (do-with-char ((fix-char *stop*)) (recurse))
      :body
      (progn
        #+nil
        (format t "~A~%" (list :stub stubmask :stop *stop*
                               :fix *fix*))
        (unless (i< *cost* *allowed-excess*) (return))
        (when (and (fix-stopp *stop*) (mask-intersect-p stubmask (fix-stemmask *stop*)))
          (funcall stop-processor))
        (unless (mask-intersect-p stubmask (iand (fix-substemmask *stop*)
                                                 (fix-substopmask *stop*))) (return))))))

(defun find-options-after-stop (*stop*)
  (let ((*cofix* *cotrie*)
        (stopmask (fix-depthmask *stop*)))
    (do-with-fix (*word* *stop*)
      :within
      (let* ((char (fix-char *word*))
             (*cofix* (step-fix char *cofix*)))
        (unless *cofix* (return))
        (do-with-char (char) (recurse)))
      :body
      (let ((cost (i+ *cost* *fix-cost*)))
	;; ---*** This is all so wrong for palindromic fixes, because their good letters
	;; are counted in the cost. Instead, we should have a special category for
	;; halves of palindromic demi-fixes, with their own subtrie, and two flags,
	;; one for palindromicity, another for oddness of length.
        (unless (i< cost *allowed-excess*) (return))
        (when (and (fix-wordp word)
                   (mask-intersect-p stopmask (fix-stopmask word)))
          (when (and (zp *sought-mask*) (fix-palindromic-p *cofix*))
            (register-solution cost word *path*))
          (iincf cost *fix-cost*)
          (unless (i< cost *allowed-excess*) (return))
          (push (make-option
                 :cost cost :sought-mask *sought-mask* :length *length*
                 :word word :stop stop :cofix *cofix*)
                *options*))
        (unless (mask-intersect-p stopmask (fix-substopmask stop)) (return))))))

(defun find-all-options ()
  (setf *options* nil)
  (walk-all-stops #'find-options-after-stop)
  *options*)


;; from the middle, we should start from both palindromic
;; prefixes and suffixes alike

(defun register-half-solution ()
  (format t "hs ~A: " *count*)
  (display-state)
  (incf *count*))

(defmacro lexicompare (left right &rest specs)
  (with-gensyms (lk rk)
    (evaluating-once (left right)
      (loop
          for ((less key) . rest) on (reverse specs)
          for result = nil then
          `(let ((,lk ,(if key `(,key ,left) left))
                 (,rk ,(if key `(,key ,right) right)))
            (cond
              ((,less ,lk ,rk) t)
              ((,less ,rk ,lk) nil)
              (t ,result)))
          finally (return result)))))

(defun fact (n)
  (loop for r = 1 then (* i r)
        for i from 2 to n
        finally (return r)))

(defun option-balance (option)
  (/ (option-cost option) (option-length option)))

(defun compare-options (opt1 opt2)
  (lexicompare opt1 opt2
               (i< option-cost)
               (i> (lambda (opt) (fix-depthmask (option-stop opt))))
               (i> (lambda (opt) (fix-depthmask (option-cofix opt))))))

(defun sort-options ()
  (setf *options* (sort *options* #'compare-options)))

(defvar *state* nil)
(defun enact-state (state)
  (multiple-value-setq
      (*trie* *cotrie* *cost* *path* *fix* *sought-mask* *pre*)
    (apply #'values state)))
(defun push-state ()
  (push (list *trie* *cotrie* *cost* *path* *fix* *sought-mask* *pre*)
        *state*))
(defun pop-state ()
  (enact-state (pop *state*)))

(defun enact-option (option)
  (psetq *trie* *cotrie*
         *cotrie* *trie*
         *cost* (option-cost option)
         *path* (cons option *path*)
         *fix* (option-cofix option)
         *sought-mask* (option-sought-mask option)
         *pre* (not *pre*)
         *options* nil))

(defmacro with-option (option &body body)
  (evaluating-once (option)
  `(let ((*options* nil)
         (*trie* *cotrie*)
         (*cotrie* *trie*)
         (*cost* (option-cost ,option))
         (*path* (cons ,option *path*))
         (*fix* (option-cofix ,option))
         (*sought-mask* (option-sought-mask ,option))
         (*pre* (not *pre*)))
    ,@body)))

(defun search-solution ()
  (incf *attempts*)
  (when (zp (logand *attempts* *attempts-mask*))
    (format t "attempt ~A " *attempts*)
    (display-state))
  (let ((*options* (find-all-options)))
    (loop for option in (sort-options) do
          (with-option option (search-solution)))))

(defun bogo-search ()
  (display-state)
  (setf *options* (find-all-options))
  (when *options* (with-option (random-option) (bogo-search))))

(defun generate-half-solutions (&optional (fix *prefix-trie*))
  (labels
      ((generate ()
	 (walk-all-stops #'process-stop))
       (process-stop ()
	 (walk-words-after-stop #'process-word))
       (process-word ()
	 (with-option (save-current-option)
	   (register-half-solution)
	   (when (i> *target-length* *length*)
	     (generate)))))
    (generate)))

(Defun try-with-limit (*allowed-excess*)
  (search-solution)
  (if *solution*
    (display-solution *solution*)
    (format t "No solution found with excess limit ~A~%" *allowed-excess*)))

;;(loop repeat 6 for i = 1 then (ceiling (* i (1+ (sqrt 5)) 1/2)) collect i)

(defun try-with-all-limits (&optional (initial-limit 1))
  (loop
      with ratio = (/ (1+ (sqrt 5)) 2)
      for limit = initial-limit then (ceiling (* ratio limit))
      for result = (try-with-limit limit)
      until result
      finally (return (first result))))


(defun solve (&optional (word-mask "abcdefghijklmnopqrstuvwxyz"))
  (let ((*sought-mask* (word-mask word-mask))
        ;;(*sought-length (mask-weight *sought-mask*))
	(*solution* nil))
    (try-with-all-limits)))


(defun print-trie-sizes ()
  (multiple-value-bind (pw pn) (fix-size *prefix-trie*)
    (multiple-value-bind (sw sn) (fix-size *suffix-trie*)
      (format t "Trie sizes:
prefix ~A words, ~A nodes; suffix ~A words (~A nodes).~%"
              pw pn sw sn))))

(defun fetch-dictionary ()
  (maybe-time "Reading the dictionary"
    (setf *dictionary* (read-dictionary))
    #+verbose (format t "Dictionary length: ~A~%" (length *dictionary*)))
  (maybe-time "Double-checking the dictionary"
    (setf *dictionary* (check-dictionary *dictionary*))
    #+verbose (format t "Checked dictionary length: ~A~%" (length *dictionary*))))

(defun setup-tries ()
  ;; in pass 0, turn the (sorted, correct) dictionary into tries
  ;; in pass 1, trim the tree by eliminating compound words
  ;; in pass 2, find direct back-stems in the trie
  ;; in pass 3, find which words complete which stop fixes.
  (maybe-time "Computing the tries"
    (setf *prefix-trie* (collect-fix-trie *dictionary*)
          *suffix-trie* (collect-fix-trie (reverse-dictionary *dictionary*)))
    #+verbose (print-trie-sizes))
  (maybe-time "Trimming the tries"
    (trim-trie *prefix-trie* *suffix-trie*)
    (trim-trie *suffix-trie* *prefix-trie*)
    #+verbose
    (print-trie-sizes))
  #+nil
  (maybe-time "Re-trimming the tries"
    (psetf *suffix-trie* (reverse-trie *prefix-trie*)
           *prefix-trie* (reverse-trie *suffix-trie*))
    #+verbose (print-trie-sizes)
    (trim-trie *prefix-trie* *suffix-trie*)
    (trim-trie *suffix-trie* *prefix-trie*)
    #+verbose (print-trie-sizes))
  (maybe-time "Finding back-stems"
    (find-back-stems *prefix-trie*)
    (find-back-stems *suffix-trie*)
    #+verbose
    (format t "~A palindromic prefixes. ~A palindromic suffixes."
            (fix-count *prefix-trie* #'fix-palindromic-p)
            (fix-count *suffix-trie* #'fix-palindromic-p)))
  (maybe-time "Finding stops"
    (find-stops *prefix-trie* *suffix-trie*)
    (find-stops *suffix-trie* *prefix-trie*)
    #+verbose
    (format t "~A stops, ~A from start."
            (fix-count *prefix-trie* #'fix-stopp)
            (fix-count *prefix-trie* #'(lambda (f) (and (fix-stopp f)
                                                        (mask-intersect-p 1 (fix-stemmask f)))))))
  (values))

(defmacro filtering-dictionary (&body body)
  `(let ((*dictionary *dictionary*))
    (setf *dictionary*
     (remove-if-not
      #'(lambda (word) (> 6 (length word)))
      *dictionary*))
    ,@body))

(defun main ()
  (fetch-dictionary)
  ;(filtering-dictionary (setup-tries))
  (setup-tries)
  (init-search))

(main)

(defun simpler ()
  (setf *dictionary-file* (merge-pathnames #p"puzzles/w" (user-homedir-pathname)))
  (main)
  (setf *sought-mask* (word-mask "banana"))
  (setf *attempts-mask* 0)
  (setf *allowed-excess* 40))

;(simpler)
