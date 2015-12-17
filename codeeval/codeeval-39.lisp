#|
https://www.codeeval.com/browse/39/

HAPPY NUMBERS
CHALLENGE DESCRIPTION:

A happy number is defined by the following process. Starting with any positive integer, replace the number by the sum of the squares of its digits, and repeat the process until the number equals 1 (where it will stay), or it loops endlessly in a cycle which does not include 1. Those numbers for which this process ends in 1 are happy numbers, while those that do not end in 1 are unhappy numbers.

INPUT SAMPLE:

The first argument is the pathname to a file which contains test data, one test case per line. Each line contains a positive integer. E.g.

1
7
22
OUTPUT SAMPLE:

If the number is a happy number, print out 1. If not, print out 0. E.g

1
1
0
For the curious, here's why 7 is a happy number: 7->49->97->130->10->1. Here's why 22 is NOT a happy number: 22->8->64->52->29->85->89->145->42->20->4->16->37->58->89 ...
|#

#|
Solution:
Let f(n) be the sum of the squares of the digits of n.
Let g(n) be 81*(1+log_10(1+n))
f(n) < g(n)
g'(n) = G/(1+n) where G=81/ln(10)
G < 36, so for n >= 35, g'(n) < 1 = f'(n).
Now, g(280) < 280, so for all n >= 280, f(n) < g(n) < n.
We can reduce this upper bound further by looking the smallest number
such that for any n >= m, f(n) < n, and for any n < m, f(n) < m.
Therefore, cycles may only involve numbers less than m.
We find it's 163, with f(99)=162.

In other words, we only have to detect all happy and unhappy numbers for n < 163,
with a union-find algorithm, and we can reduce the problem to iterating f until
the number is less than 163.

Actually, we only look for the happy numbers (i.e. one cycle, not all of them),
and only below 163, so we can use a simple bitmap and walk the reverse table
with a simple queue starting backwards from 1.
Any correct algorithm for n<163 is ipso facto O(1), and
the reduction to the small problem by iteration is pretty fast.
So we could have a trivial iterate-and-detect-cycle algorithm
that would do about as well.
|#

(uiop:define-package :fare-puzzles/codeeval/codeeval-39
  (:use :fare-utils :uiop :cl
        :fare-puzzles/util/decimal)
  (:export #:happyp #:main))

(in-package :fare-puzzles/codeeval/codeeval-39)

;;(uiop-debug) (declaim (optimize (speed 1) (safety 3) (debug 3))) ; DEBUG!
;;(declaim (optimize (speed 3) (safety 0) (debug 0))) ;; GO FAST!

(defun f (n) (loop :for d :in (digits-of n) :sum (* d d)))
(defun g (n) (* 81 (1+ (log (1+ n) 10))))

(assert (< (g 280) 280))
(assert (< (/ 81 (log 10) (1+ 35)) 1))
(assert (< (/ (- (g 35.001) (g 35)) .001) 1))

(defparameter *m*
  (1+ (loop for i below 280 for j = (f i) when (<= i j) maximize j))
  "Smallest number such that for any n >= m, f(n) < n, and for any n < m, f(n) < m.
Therefore, cycles may only involve numbers less than m.")

(defun happyp/naive (n)
  "Naive, straightforward solution. We have proven that it works"
  (loop :for l = () :then (cons i l)
        :for i = n :then (f i)
        :until (member i l :test 'equal)
        :finally (return (= i 1))))

(defparameter *antecedents*
  (let ((antecedents (make-array *m* :initial-element nil)))
    (loop :for n :below *m* :do (push n (aref antecedents (f n))))
    antecedents))

(defun visit-antecedents (n visited)
  (loop :with queue = (list n) :while queue :do
    (destructuring-bind (a &rest more) queue
      (if (zerop (aref visited a))
          (setf (aref visited a) 1
                queue (append (aref *antecedents* a) more))
          (setf queue more)))))

(defparameter *happyp*
  (let ((happyp (make-array *m* :initial-element 0 :element-type 'bit)))
    (visit-antecedents 1 happyp)
    happyp))

(defun happyp (n)
  "Faster solution"
  (check-type n (integer 0 *))
  (loop :for i = n :then (f i)
        :until (< i *m*)
        :finally (return (plusp (aref *happyp* i)))))

;; OK, what if we want to find all the cycles?
(defparameter *cycle-minima*
  (loop
    :with visited = (make-array *m* :initial-element 0 :element-type 'bit)
    :for i :below *m*
    :when (zerop (aref visited i))
      :collect i
    :and
      :do (visit-antecedents i visited)))

(assert (equal *cycle-minima* '(0 1 2 3 4)))

(defun happyp/clever (n)
  "Clever solution. We have proven that it works"
  (loop :for i = n :then (f i)
        :until (< i 5)
        :finally (return (= i 1))))

(defun main ()
  (loop :for l = (read-line nil nil)
        :while l
        :do (println (if (happyp (parse-integer l)) 1 0)))
  t)
