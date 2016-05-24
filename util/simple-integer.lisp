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
  (* n (/ m (lcm n m))))

(defun isqrt.1 (n)
  (check-type n (integer 0 *))
  (if (<= n 1) n
      (loop
	;; Compute with two extra bits for n,
	;; to keep termination simple while avoid bad rounding effects.
	;; Note that if we wanted to optimize the fixnum case, then
	;; we might want to treat specially numbers too big to fit those two extra bits in a fixnum,
	;; and determine their last bit using dichotomy.
	:with n4 = (ash n 2)
	:with p = (ash n4 (- 1 (ash (integer-length n) -1)))
	:for q = (floor n4 p)
	:while (> p q)
	:do (setf p (floor (+ p q) 2))
	:finally (return (ash p -1)))))
