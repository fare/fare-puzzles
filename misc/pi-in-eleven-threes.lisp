(uiop:define-package :fare-puzzles/misc/pi-in-eleven-threes
  (:use :common-lisp ::fare-puzzles/util/simple-comb))
(in-package :fare-puzzles/misc/pi-in-eleven-threes)

#|
Reposting a math puzzle from Scott Garrabrant with a slight restatement.
Using up to 11 times the number "3", additions, subtractions, multiplications, divisions, exponentiations, and integer factorials (no gamma) how closely can you approximate pi?
hint: it's a *math* puzzle
-- Arthur Breitman
|#

;; Make n and m each sufficiently large iteratees of fact 3.
;; we invert Stirling's formula, with 1/e = (1-1/m)^m
;; (factorial n) == (* (sqrt (* 2 pi n)) (exp (/ n e) n))
;;
;; pi == (/ (exp (/ (factorial n) (exp (/ n e) n)) 2) n 2)
;; We use (/ (factorial 3) 3) for 2 and
;; we use (exp (/ (- 3 (/ 3 m)) 3) m) for 1/e
;;
;; Takes 13 terminals
(defun almost-pi-13 (n m k)
  (/ (expt (/ (factorial n) (expt (* n (expt (/ (- k (/ k m)) k) m)) n)) (/ (factorial 3) 3))
     n (/ (factorial 3) 3)))

;; Double precision goes to ~1d-16 of precision, so this function tops precision at m ~1d9.
(defun almost-1/e (m) (expt (- 1d0 (/ m)) m))

;; Only 10 terminals http://imgur.com/a/5MDSj
;; We "need" double-precision floating point numbers due to fractional exponent, at which point
;; the top factorial iteratee of three that doesn't explode the double-float format is 3!
;; for an approximation of pi by 3.26.
(defun almost-pi-10 (n)
  (/ (expt (* (factorial n) (expt 3 (+ n (/ 3d0 (factorial 3))))) 3)
     (factorial 3) (factorial (* 3 n)) n))
