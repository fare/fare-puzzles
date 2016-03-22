":" ; PATH=$(dirname "$0"):$PATH cl-launch -f "$0" -E fare-puzzles/words/concordance::main -- "$@" ; exit
#| ;-*- Lisp -*-


How to use this program
-----------------------

At the Lisp REPL:

    (require "asdf") ;; ASDF 3.1.2 or later is required.
    (load "concordance.lisp") ;; or (asdf:make :fare-puzzles/words/concordance)
    (in-package :fare-puzzles/words/concordance)

    (concordance "some string")
    (bonus "some string")
    (bonus "some string" :skip-numbering t)


From the shell command line, assuming all your CL source code is properly
configured including all dependencies, and cl-launch is in your PATH:

    ./concordance.lisp help
    ./concordance.lisp < input.text
    ./concordance.lisp condordance - < input.text
    ./concordance.lisp bonus input.text
    ./concordance.lisp bonus_no_numbering input.text


If everything is installed at expected paths, you can also dump an image with:

    cl-launch -o ./concordance -d '!' \
	-f concordance.lisp \
	-E fare-puzzles/words/concordance::main


Puzzle statement
----------------

The original puzzle statement is as follows:
"""
Given an arbitrary text document written in English, write a program that will generate a
concordance, i.e. an alphabetical list of all word occurrences, labeled with word frequencies.
Bonus: label each word with the sentence numbers in which each occurrence appeared.

a.	a	{2:1,1}
b.	all	{1:1}
c.	alphabetical	{1:1}
d.	an	{2:1,1}
e.	appeared	{1:2}
f.	arbitrary	{1:1}
g.	bonus	{1:2}
h.	concordance	{1:1}
i.	document	{1:1}
j.	each	{2:2,2}
k.	english	{1:1}
l.	frequencies	{1:1}
m.	generate	{1:1}
n.	given	{1:1}
o.	i.e.	{1:1}
p.	in	{2:1,2}
q.	label	{1:2}
r.	labeled	{1:1}
s.	list	{1:1}
t.	numbers	{1:2}
u.	occurrence	{1:2}
v.	occurrences	{1:1}
w.	of	{1:1}
x.	program	{1:1}
y.	sentence	{1:2}
z.	text	{1:1}
aa.	that	{1:1}
bb.	the	{1:2}
cc.	which	{1:2}
dd.	will	{1:1}
ee.	with	{2:1,2}
ff.	word	{3:1,1,2}
gg.	write	{1:1}
hh.	written	{1:1}
"""

[Do not read what follows until after you've tried to solve the puzzle yourself,
or at least have thought hard about all the subtleties involved in a solution.]


Puzzle discussion
-----------------

This recruitment puzzle is both simple in its algorithmic part (counting), yet
with infinite room for improvement in its heuristic part (natural language
parsing). It is also subtly ambiguous in its requirement: on the one hand,
it requests that one should output _frequencies_ (not occurrence counts),
but doesn't provide sample output; on the other hand, it then requests some
additional data as bonus, for which it _does_ provide it but it gives an
example output only for the bonus data that includes occurrence counts but
not frequencies. Finally, that sample output includes a numbering scheme that
doesn't scale to large word corpuses (hh.).

In a real-world work situation, requirements are often specified with
insufficient coherence or precision, and it is necessary for the multiple
parties to negotiate what exactly is needed. In the case of a recruitment
puzzle, it can be assumed that such requirements are intentionally left
imprecise so as to test how the candidate will react. The proper reaction is
to answer as you would in a work situation (and would expect colleagues to do),
by simultaneously doing a best effort at providing a solution to the problem
as stated so far, yet pin-pointing all the places when the problem statement or
its solution could be improved.

I chose to provide solutions to the main concordance question and the bonus
question as separate functions. I also chose to have the bonus function
precisely mimic the sample output provided, modulo use of whitespace rather
than PDF formatting. Yet, I also provided an alternate solution to the
bonus question without the numbering tag, in addition to a version with the
suggested numbering tag, because said tag seems to follow a unary encoding
that can't scale. Finally, I peppered the source code with many TODO
comments on how the code could be improved.

|#

(uiop:define-package :fare-puzzles/words/concordance
  (:use :cl :uiop)
  (:export #:concordance))

(in-package :fare-puzzles/words/concordance)

(declaim (optimize (speed 1) (debug 3) (safety 3))) ;; better for debugging
;;(declaim (optimize (speed 3) (debug 1) (safety 2))) ;; if you want to optimize a bit

(defparameter *test-input*
  "Given an arbitrary text document written in English, write a program that will generate a
concordance, i.e. an alphabetical list of all word occurrences, labeled with word frequencies.
Bonus: label each word with the sentence numbers in which each occurrence appeared.")

(defparameter *dotted-words*
  ;; TODO: compile a longer list of such words, or
  ;; a better general algorithm for distinguishing sentence-ending full stops
  ;; from dots that are parts of abbreviations.
  '("Mr." "Mrs." "Mssrs." "Ms." "Dr." "i.e.")
  "A list of specially accepted words containing dots.")

(defun letterp (x)
  "Is X a valid letter?"
  ;; TODO: properly handle non-ASCII letters.
  (and (characterp x) (or (char<= #\a x #\z) (char<= #\A x #\Z))))

(defun sentence-ending-punctuation-p (x)
  "Is X a sentence-ending punctuation character?"
  (and (find x ".!?") t))

(defun letter-or-sentence-ending-punctuation-p (x)
  "Is X either a letter or a sentence-ending punctuation character?"
  (or (letterp x) (sentence-ending-punctuation-p x)))

(defun string-case-insensitive-prefix-p (prefix string &key start)
  "Is PREFIX a prefix of string at start START when doing a case-insensitive comparison?"
  (let ((end (+ start (length prefix))))
    (when (<= end (length string))
      (string-equal prefix string :start2 start :end2 end))))

(defun get-next-word-or-punctuation (string start)
  "Given a string and a start index, return three values:
the next word or sentence-ending punctuation sign in the string (case preserved), a string;
the index for the first character after the word in the string, an integer;
and whether that substring was a word (T) or a sentence-ending punctuation (NIL), a boolean.
If the end of the string is reached without finding a word or punctuation, return NIL, NIL and NIL."
  ;; This is a very heuristic function, that has infinite room for improvement.
  ;; This initial version is barely good enough to handle the given example.
  ;; A perfect version of this function is unhappily AI-complete.
  ;; TODO: Improve this function as needed.
  ;; TODO: Write a perfect AI.
  (block :g
    (labels ((not-found ()
               (return-from :g (values nil nil nil)))
             (found (start length wordp)
               (let ((end (+ start length)))
                 (return-from :g (values (subseq string start end) end wordp)))))
      (unless start (not-found))
      (let ((word-start
              (position-if #'letter-or-sentence-ending-punctuation-p string :start start)))
        (unless word-start (not-found))
        ;; Specially recognize sentence-ending punctuation: .!?
        ;; TODO: recognize ... or its unicode short-cut as either ending a sentence or not.
        (when (sentence-ending-punctuation-p (char string word-start))
          (found word-start 1 nil))
        ;; Specially recognize special words that contain dots, i.e. "i.e.".
        ;; TODO: optimize this by using a pre-computed prefix trie.
        (loop :for w :in *dotted-words* :do
          (when (string-case-insensitive-prefix-p w string :start word-start)
            (found word-start (length w) t)))
        ;; Normal case: a word is a series of letters.
        ;; TODO: recognize non-English words as series of letters in the same alphabet.
        ;; TODO: recognize composite words such as "non-English".
        ;; TODO: recognize apostrophe as part of an abbreviated word, as in "ol'" -- or French "l'"
        (let ((word-end (position-if-not #'letterp string :start (1+ word-start))))
          (found word-start (- word-end word-start) t))))))

(defun process-string (processor string)
  "Process STRING by calling the PROCESSOR function with two arguments SUBSTRING and WORDP
for each word or sentence-ending punctuation in the STRING, with SUBSTRING being a substring,
and WORDP being a boolean indicating if the substring was a word (or else, sentence-ending
punctuation)."
  (loop :with start = 0 :do
    (multiple-value-bind (word new-start wordp) (get-next-word-or-punctuation string start)
      (unless word (return))
      (funcall processor word wordp)
      (setf start new-start))))

;; Just a simple way to test the above parser.
(defun test-parse ()
  (process-string #'(lambda (w wp) (format t "~A: ~A~%" (if wp "Word" "SEP") w))
                  *test-input*))

(defun normalize-word (word)
  "Given a WORD as appears in the input text, normalize it.
The current version of this function just downcases the word."
  ;; TODO: be more clever about normalizing proper nouns, acronyms, etc.
  (string-downcase word))

(defun hash-table-keys (table)
  "Given a hash-table TABLE, return a list of the keys of entries in the table"
  ;; NB: we could use alexandria:hash-table-keys, but this way we avoid a dependency.
  (loop :for key :being :the :hash-keys :of table :collect key))

(defun concordance (string)
  "Given a STRING of English text, for each word in the string, sorted lexicographically,
output a line with the word, space separation and the frequency of the word in the text."
  (let ((occurrence-count (make-hash-table :test 'equal))
        (total-count 0))
    (flet ((process-word (word wordp)
             (when wordp
               (incf (gethash (normalize-word word) occurrence-count 0))
               (incf total-count))))
      (process-string #'process-word string)
      (loop :for word :in (sort (hash-table-keys occurrence-count) 'string<)
            :for count = (gethash word occurrence-count)
            :for frequency = (coerce (/ count total-count) 'double-float) :do
              ;; TODO: adapt word length and frequency precision to input text
              (format t "~14A ~F~%" word frequency)))))

(defun unary-letter-numbering (n)
  "Given a zero-based index N, return a string for (N+1)th item in a list,
using letters [a-z] for the 26 first entries, then [a-z] doubled for the next 26 entries,
then tripled, then repeated n/26 times in a unary numbering."
  ;; TODO: negotiate with the client a less ridiculous way of numbering words,
  ;; that would scale to larger corpus of words. Then get rid of this function.
  (multiple-value-bind (repeats letter-index) (floor n 26)
    (make-string (1+ repeats) :initial-element (code-char (+ #.(char-code #\a) letter-index)))))

(defun bonus (string &key skip-numbering)
  "Given a STRING of English text, for each word in the string, sorted lexicographically,
output a line with two or three whitespace-separated entries: a numbering tag (present only
if SKIP-NUMBERING is false, the default), the word, and in curly braces a count of occurrences,
a colon, and a comma separated list of 1-based indexes of sentences in which the word occurred."
  (let ((occurrence-list (make-hash-table :test 'equal))
        (sentence-index 1))
    (flet ((process-word (word wordp)
             (if wordp
                 (push sentence-index (gethash (normalize-word word) occurrence-list '()))
                 (incf sentence-index))))
      (process-string #'process-word string)
      (loop
        :for word :in (sort (hash-table-keys occurrence-list) 'string<)
        :for occurrences = (reverse (gethash word occurrence-list)) ; in appearance order
        :for count = (length occurrences)
        :for index :from 0
        :for tag = (unless skip-numbering (strcat (unary-letter-numbering index) ".")) :do
          ;; TODO: negotiate a less absurd output numbering format.
          ;; TODO: adapt field lengths to input instead of using arbitrary precomputed lengths.
          (format t "~@[~4A ~]~14A  {~A:~{~A~^,~}}~%" tag word count occurrences)))))


;;;; Command-line interface for the puzzle solution

(defun display-help (&optional (output *standard-output*))
  "Display help for the command-line version of this utility"
  (format output "Usage:
  concordance < input.text
  concordance concordance < input.text
  concordance concordance - < input.text
  concordance concordance input.text
    for each word in the English text, sorted lexicographically,
    output a line with the word, space separation and the frequency of the word in the text.

  concordance bonus < input.text
  concordance bonus - < input.text
  concordance bonus input.text
    for each word in the English text, sorted lexicographically,
    output a line with three space-separated entries: a numbering tag, the word,
    and in curly braces a count of occurrences, a colon, and a comma separated list of
    1-based indexes of sentences in which the word occurred.

  concordance bonus_no_numbering < input.text
  concordance bonus_no_numbering - < input.text
  concordance bonus_no_numbering input.text
    same as with bonus, but without the numbering tag, because it doesn't scale.
    We propose to make that the default output format for the bonus question.
"))

(defun bad-arguments ()
  "Error out when the utility was called with bad arguments."
  (format *error-output* "Bad arguments.~%~%")
  (display-help *error-output*)
  (quit 2))

(defun get-input-data (arguments)
  "Given the command-line ARGUMENTS, return the input data as a string."
  (cond
    ((third arguments)
     (bad-arguments))
    ((and (second arguments) (not (string= (second arguments) "-")))
     (read-file-string (second arguments)))
    (t
     (loop :for l = (read-line *standard-input* nil nil)
           :while l :collect l :into lines
           :finally (return (format nil "~{~A~%~}" lines))))))

(defun main (arguments)
  "Main entry function" ;; to use with cl-launch ... -E main
  (let ((command (first arguments)))
    (cond
      ((or (null command) (string-equal command "concordance"))
       (concordance (get-input-data arguments)))
      ((string-equal command "bonus")
       (bonus (get-input-data arguments)))
      ((string-equal command "bonus_no_numbering")
       (bonus (get-input-data arguments) :skip-numbering t))
      ((member command '("help" "-h" "--help") :test 'string-equal)
       (display-help))
      (t
       (bad-arguments)))))
