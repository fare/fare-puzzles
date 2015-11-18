(uiop:define-package :fare-puzzles/util/cache
  (:use :uiop :cl)
  (:export #:defcache #:defsequence))

(in-package :fare-puzzles/util/cache)

;;; Since we're dealing with recursively defined sequences,
;;; let's define utilities to memoize the start of such sequences.
(defmacro defcache (name (&key (element-type t)) &rest initial-contents)
  `(defparameter ,name
     (make-array '(,(length initial-contents))
                 :initial-contents ',initial-contents
                 :element-type ,element-type
                 :adjustable t :fill-pointer t)))

(defmacro defsequence ((name cache) (n) &body body)
  `(defun ,name (,n)
     (check-type ,n (integer 0 *))
     (if (< ,n (fill-pointer ,cache))
         (aref ,cache ,n)
         (let ((value (progn ,@body)))
           (vector-push-extend value ,cache)
           value))))
