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
Discussion:
Let f(n) be the sum of the squares of the digits of n.
Let g(n) be 81*(1+log_10(1+n))
f(n) < g(n)
g'(n) = G/(1+n) where G=81/ln(10)
G < 36, so for n >= 35, g'(n) < 1 = f(n).
Now, g(280) < 280, so for all n >= 280, f(n) < g(n) < n.

In other words, we only have to detect all happy and unhappy numbers for n < 280
with a union-find algorithm, and we can reduce the problem to iterating f until
the number is less than 280.
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
(defparameter *m* 280) ;; see explanation above

(defparameter *antecedents*
  (let ((antecedents (make-array *m* :initial-element nil)))
    (loop :for n :below *m* :do (push n (aref antecedents (f n))))
    antecedents))

(defparameter *happyp*
  (let ((happyp (make-array *m* :initial-element 0 :element-type 'bit)))
    (loop :with queue = '(1) :while queue :do
      (destructuring-bind (happy &rest more) queue
        (if (zerop (aref happyp happy))
            (setf (aref happyp happy) 1
                  queue (append (aref *antecedents* happy) more))
            (setf queue more))))
    happyp))

(defun happyp (n)
  (check-type n (integer 0 *))
  (loop :for i = n :then (f i)
        :until (< i *m*)
        :finally (return (plusp (aref *happyp* i)))))

(defun main ()
  (loop :for l = (read-line nil nil)
        :while l
        :do (println (if (happyp (parse-integer l)) 1 0)))
  t)
