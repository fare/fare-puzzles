;; Simple combinatorics
(uiop:define-package :fare-puzzles/util/simple-comb
  (:use :uiop :cl)
  (:export
   #:factorial
   #:arrangements
   #:combinations
   ))

(in-package :fare-puzzles/util/simple-comb)

(defun arrangements (n m)
  (check-type n (integer 0 *))
  (check-type m (integer 0 *))
  (assert (<= m n))
  (loop for i from (- n m) to n
        for p = 1 then (* p i)
        finally (return p)))

(defun factorial (n)
  (arrangements n n))

(defun combinations (n m)
  (/ (arrangements n m) (factorial m)))
