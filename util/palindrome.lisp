;;; Dealing with palindromes, particularly in decimal number representation
(uiop:define-package :fare-puzzles/util/palindrome
  (:use :uiop :cl
        :fare-puzzles/util/decimal)
  (:export
   #:sequence-palindrome-p
   #:palindromep))
(in-package :fare-puzzles/util/palindrome)

(defun sequence-palindrome-p.1 (l)
  (equal l (reverse l)))

(defun sequence-palindrome-p (l)
  (loop with v = (coerce l 'vector)
        with len = (length v)
        for i from 0 below (ash len -1)
        always (equal (aref v i) (aref v (- len 1 i)))))

(defun palindromep.1 (n)
  (sequence-palindrome-p (digits-of n)))

(defun palindromep (n &optional (l (number-of-digits n)) (p (expt 10 l)))
  (nest
   (loop)
   (if (or (< n 0) (<= p n)) (return nil))
   (if (< l 2) (return t))
   (multiple-value-bind (q r) (floor n 10))
   (setf p (floor p 100)
         l (- l 2)
         n (- q (* r p)))))
