;; Simple integer arithmetics
(uiop:define-package :fare-puzzles/util/simple-integers
  (:use :uiop :cl)
  (:export
   #:numbers-below-n))

(in-package :fare-puzzles/util/simple-integers)

(defun numbers-below-n (n)
  (loop for i below n collect i))

