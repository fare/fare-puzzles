#|
Problem Statement
-----------------

Given a substitution from character to string, e.g. #\A -> "1", #\B -> "2", ... #\Z -> "26",
count the number of ways a given string could have been reached, e.g. "111" could come from "AAA", "AK" or "KA",
so the count of ways to reach it is 3.

Discussion
----------

We are counting the number of successful parses from a particular non-deterministic finite automaton,
of the form A* (indefinite repetitions of A) where A is one of "1" | "2" | "3" | ...
This could be generalized to arbitrary non-deterministic finite automata, except with N*M variables
where N is the number of states in the automaton, and M is the lookahead required for state transitions.
|#

(uiop:define-package :fare-puzzles/misc/count-astar
  (:mix :common-lisp :uiop :alexandria))

(cl:in-package :fare-puzzles/misc/count-astar)

(defparameter *replacements*
  (alist-hash-table
   (loop
     :for i :from 1 :to 26
     :for code :from (char-code #\A)
     :for char = (code-char code)
     :for string = (princ-to-string i)
     :collect (cons char string))
   :test 'equal)
  "Rewrite rules from alphabet of letters to sequences of digits")

(defun replace-alphabet (string-of-letters)
  "Given a string of letters, expand into a string of digits."
  (with-output-to-string (o)
    (loop :for letter :across string-of-letters
      :for replacement = (gethash letter *replacements*) :do
      (assert replacement)
      (princ replacement o))))

(defun substring-match (string start end pattern pattern-length)
  (let ((end1 (+ start pattern-length)))
    (and (<= end1 end)
	 (string= string pattern :start1 start :end1 end1))))

(defun count-preimages (string-of-digits)
  "Given a string of digits, count the string."
  (loop
    :with image-length = (length string-of-digits)
    :for (current ahead1 ahead2) = '(1 0 0) then (list ahead1 ahead2 0)
    :for index :from 0 :below image-length
    :when (plusp current) :do
    (loop :for replacement :being :the :hash-values :of *replacements*
      :for replacement-length = (length replacement)
      :when (substring-match string-of-digits index image-length replacement replacement-length)
      :do (ecase replacement-length
	    ((1) (incf ahead1 current))
	    ((2) (incf ahead2 current))))
    :finally (return current)))
