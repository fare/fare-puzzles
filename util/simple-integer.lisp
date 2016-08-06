;; Simple integer arithmetics
(uiop:define-package :fare-puzzles/util/simple-integer
  (:use :uiop :cl)
  (:export
   #:integers-below))

(in-package :fare-puzzles/util/simple-integer)

(defun integers-below (n)
  (loop for i below n collect i))

(defun gcd.1 (n m)
  (loop while (plusp n) do
        (psetq n m m (mod n m))
        finally (return m)))

(defun lcm.1 (n m)
  (* n (/ m (gcd n m))))

(defun isqrt.1 (n)
  (check-type n (integer 0 *))
  ;; This function computes ISQRT, using the Newton-Raphson method,
  ;; which doubles the number of valid digits at every iteration
  ;; (so should have only 6 iterations for a 64-bit number).
  ;; We compute with one extra bit for the candidate square root (two extra bits for n)
  ;; to keep termination simple while avoid bad rounding effects.
  ;; Someone who'd want to use only fixnums when provided fixnum input might instead
  ;; strip one bit when looking for termination, then use dichotomy for the last bit.
  ;; Considering the speed of FPUs these days, we would probably also do better
  ;; by using SQRT to compute the first 53 bits of mantissa instead of shifting the bits of n
  ;; to the right.
  (if (<= n 1) n
      (loop
	:with nx4 = (ash n 2)
	:with first-approx = (ash nx4 (- 1 (ash (integer-length n) -1)))
	:for p = first-approx :then (ash (+ p q) -1)
	:for q = (floor nx4 p)
	:while (> p q)
	:finally (return (ash p -1)))))
