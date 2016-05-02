#| Given a list of N numbers from 1 to N-1, find the first duplicate.
|#
(defpackage :fare-puzzles/misc/duplicate
  (:use :common-lisp)
  (:import-from :alexandria #:shuffle))

(in-package :fare-puzzles/misc/duplicate)

(defparameter *l*
  (shuffle (cons 700 (loop for i from 1 to 999 collect i))))

(defun process-number (state number victory-continuation)
   (if (gethash number state)
       (funcall victory-continuation number)
       (setf (gethash number state) t)))

(defun process-list (list)
  (let ((state (make-hash-table :test 'equal)))
    (block ()
      (labels ((victory (number) (return number)))
	(loop for number in list do (process-number state number #'victory))))))
