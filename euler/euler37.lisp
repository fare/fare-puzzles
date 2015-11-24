#|
Truncatable primes
Problem 37
The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
|#

(uiop:define-package :fare-puzzles/euler/euler37
  (:use :fare-utils :uiop :cl
        :fare-puzzles/util/decimal :fare-puzzles/util/simple-prime)
  (:export #:solution))

(in-package :fare-puzzles/euler/euler37)

;;(uiop-debug) (declaim (optimize (speed 1) (safety 3) (debug 3))) ; DEBUG!
;;(declaim (optimize (speed 3) (safety 0) (debug 0))) ;; GO FAST!

(defun solution ()
  (loop
    for previous-length from 1
    for previous-left = (reverse *prime-digits*) then left-truncatable
    for previous-right = (reverse *prime-digits*) then right-truncatable
    for left-truncatable = (next-left-truncatables previous-length previous-left)
    for right-truncatable = (next-right-truncatables previous-length previous-right)
    for current-solutions = (sorted (intersection left-truncatable right-truncatable :test 'equal) '<)
    while (and left-truncatable right-truncatable t)
    ;; do (DBG :s left-truncatable right-truncatable current-solutions
    do (map () 'println current-solutions)
    sum (reduce '+ current-solutions)))

(defun next-left-truncatables (length left-truncatables)
  ;; LENGTH is length in base 10 of current solutions.
  ;; LEFT-TRUNCATABLES is left-truncatables of given LENGTH in decreasing order.
  ;; Try the potential solutions in decreasing order, to minimize sieving.
  (while-collecting (c)
    (loop for digit from 9 downto 1 do
      (loop for truncatable in left-truncatables
            for candidate = (+ (* (power-of-ten length) digit) truncatable) do
              (when (small-prime-p candidate)
                (c candidate))))))

(defun next-right-truncatables (length right-truncatables)
  ;; LENGTH is length in base 10 of current solutions.
  ;; RIGHT-TRUNCATABLES is right-truncatables of given LENGTH in decreasing order.
  ;; Try the potential solutions in decreasing order, to minimize sieving.
  (declare (ignore length))
  (while-collecting (c)
    (loop for truncatable in right-truncatables do
      (loop for digit from 9 downto 1
            for candidate = (+ (* 10 truncatable) digit) do
              (when (small-prime-p candidate)
                (c candidate))))))
