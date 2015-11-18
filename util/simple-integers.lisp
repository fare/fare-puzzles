;; Simple integer arithmetics
(uiop:define-package :fare-puzzles/util/simple-integers
  (:use :uiop :cl)
  (:export
   #:numbers-below-n))

(in-package :fare-puzzles/util/simple-integers)

(defun numbers-below-n (n)
  (loop for i below n collect i))

(defun gcd.1 (n m)
  (loop while (plusp n) do
        (psetq n m m (mod n m))
        finally (return m)))

(defun lcm.1 (n m)
  (* n (/ m (lcm n m))))
