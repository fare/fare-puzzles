;;; Phone chess problem
;; Given a phone's number pad:
;; 1 2 3
;; 4 5 6
;; 7 8 9
;;   0
;; What are the seven-digit numbers you can make with a knight moving on the keyboard?
;; See comments at the end.
;;
(uiop:define-package :fare-puzzles/misc/phone-chess
  (:use :common-lisp)
  (:import-from :uiop #:nest #:while-collecting)
  (:import-from :alexandria #:alist-hash-table))

(cl:in-package :fare-puzzles/misc/phone-chess)

(defparameter *next-moves*
  (alist-hash-table
   '((1 6 8)
     (2 7 9)
     (3 4 8)
     (4 3 9 0)
     (5)
     (6 1 7 0)
     (7 1 6)
     (8 1 3)
     (9 2 4)
     (0 4 6))
   :test 'equal)
  "Allowed transitions given the keypad and knight move")

(defun state-position (state)
  "Given a state of the exploration, find the current position."
  (first state))

(defun state-solution (state)
  "Given a state of the exploration state, extract a human-readable solution."
  (loop :for digit :in state
    :for factor = 1 :then (* 10 factor)
    :sum (* digit factor)))

(defun next-moves (state)
  (gethash (state-position state) *next-moves*))

(defun next-state (state move)
  (cons move state))

(defun depth-first-move-sequences (depth states collector)
  (nest
   (if (zerop depth) (map () collector states))
   (loop for state in states do)
   (loop for move in (next-moves state)
	 for next-state = (next-state state move) do)
   (depth-first-move-sequences (1- depth) (list next-state) collector)))

(defun breadth-first-move-sequences (depth states collector)
  (if (zerop depth)
      (map () collector states)
      (breadth-first-move-sequences
       (1- depth)
       (while-collecting (collect-state)
	 (loop for state in states do
	       (loop for move in (next-moves state) do
		     (collect-state (next-state state move)))))
       collector)))

(defun phone-chess-depth-first (start depth)
  (while-collecting (collect-move)
    (depth-first-move-sequences
     (1- depth)
     `((,start))
     (lambda (state) (collect-move (state-solution state))))))

(defun phone-chess-breadth-first (start depth)
  (while-collecting (collect-move)
    (breadth-first-move-sequences
     (1- depth)
     `((,start))
     (lambda (state) (collect-move (state-solution state))))))

#|
(phone-chess-depth-first 1 7)
(phone-chess-breadth-first 1 7)

I fumbled a bit in the details on the whiteboard.
Writing code on the computer makes it more concrete.
When coding, I tend to assume the computer as an aid and safety net,
so I can both focus on the abstract yet be reality-checked for the concrete.

Suggestions I failed to properly convey:

* If you're going to be doing a lot of search and generation,
  use a non-deterministic and/or a probabilistic programming language (or at least, monad)
  that does the state and traversal management for you;
  then you don't get messed up like I did during the interview.

* The solution would look simply like this, focusing on the problem at hand
  without having to care about how to implement non-determinism:

(defun phone-chess (state depth)
  (if (zerop depth)
      (state-solution state)
      (phone-chess (next-state state (pick-one (next-moves state))) (1- depth))))

* One simple way to implement non-determinism is when your language has first-class
  continuations (if possible delimited continuations), at which point
  pick-one can be implemented as capturing the continuation, then
  calling it for each element in the parameter list.
  This does "inversion of control", so that each of the phone-chess program
  and the traversal strategy (including parallelism) can be written in "direct style"
  without having to care about carrying state for the other part, and can be modified modularly.

* If your language doesn't have first-class continuations, an equivalent but much heavier
  solution is to use monads to implement them and/or to directly implement one side
  of the language (say, non-determinism with a depth-first traversal strategy).
  Writing depth-first vs breadth-first in direct style can be just switching the work queue
  from a LIFO to a FIFO queue. Or a priority queue based on expectation of return
  from each choice, etc.

* Similarly, memoization can be done simple using fare-memoization:define-memo-function
  instead of defun for breadth-first-move-sequences and/or depth-first-move-sequences.
  In Scala, you could have an annotation @memoize or some such.

* You can compactly represent the solutions as a finite state machine, or equivalently, as a regular expression.
|#
