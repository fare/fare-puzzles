;; Simple general search functions
(uiop:define-package :fare-puzzles/util/simple-search
  (:use :uiop :cl)
  (:export
   #:dichotomy-search
   #:find-majorant
   #:find-first))

(in-package :fare-puzzles/util/simple-search)

;;; We could compute the number of digits by dichotomy.
(defun dichotomy-search (pred start end)
  "Given a predicate PRED over integers, that is false for START and subsequent values,
until it becomes true for all values, and is true for END, where START < END,
find the smallest integer for which it is true."
  (nest
   (loop)
   (let ((mid (ash (+ start end) -1))))
   (if (= mid start) (return end))
   (if (funcall pred mid)
       (setf end mid)
       (setf start mid))))

(defun find-majorant (pred)
  "Given a predicate on integers that is false for small integers including 0
then becomes true for large enough integers,
find a positive integer that makes it true, less than double the smallest one"
  (nest
   (loop for n = 1 then (ash n 1) do)
   (if (funcall pred n) (return n))))

(defun find-first (pred)
  "Given a predicate on integers that is false for small integers including 0
then becomes true for large enough integers,
find the smallest non-negative integer that makes it true"
  (let ((end (find-majorant pred)))
    (dichotomy-search pred 0 end)))

