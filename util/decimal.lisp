;; Dealing with decimal representation of integers
(uiop:define-package :fare-puzzles/util/decimal
  (:use :uiop :cl
        :fare-puzzles/util/cache :fare-puzzles/util/simple-search)
  (:export
   #:power-of-ten
   #:power-of-ten-p
   #:digits-of
   #:number-of-digits

   #:*prime-digits*
   #:invert-modulo-10k ;; move to a separate file on modulo-10^k arithmetics?
   ))

(in-package :fare-puzzles/util/decimal)

;;; First, we're going to play a lot with small powers of 10, so let's make that easy.
(defcache *powers-of-ten* () 1)
(defsequence (power-of-ten *powers-of-ten*) (n)
  (* (power-of-ten (1- n)) 10))

(defun power-of-ten-p (n)
  (equal n (expt 10 (1- (number-of-digits n)))))

(defun digits-of (n)
  (check-type n (integer 0 *))
  (loop with l = '() do
    (multiple-value-bind (q r) (floor n 10)
      (push r l)
      (if (zerop q) (return l) (setf n q)))))

(defun number-of-digits.1 (n)
  (check-type n (integer 0 *))
  (loop for i from 1 when (< n (power-of-ten i)) return i))

(defun number-of-digits.2 (n)
  (check-type n (integer 0 *))
  (find-first (lambda (i) (< n (power-of-ten i)))))

(defun number-of-digits (n)
  ;;; This function works well enough for all the numbers we care about,
  ;;; but let's not let it silently give the wrong answer.
  (assert (< (integer-length (integer-length n)) 50)) ;; NB: a double's mantissa is 53 bits long.
  (ceiling (log (+ 1 n) 10d0)))


(defparameter *prime-digits* '(2 3 5 7))

(defun invert-modulo-10k (k a)
  (nest
   (case k
     ((0) 0)
     ((1) (ecase a (1 1) (3 7) (7 3) (9 9))))
   (otherwise)
   (let* ((l (ceiling k 2))
          (p (power-of-ten l))
          (q (power-of-ten (- k l)))))
   (multiple-value-bind (a-high a-low) (floor a p)) ;; a = ah*p+al
   ;; (ah*p+al)*(bh*p+bl) = 1 [p^2]
   (let* ((b-low (invert-modulo-10k l a-low)) ;; bl*al = 1 [p]
          (r (floor (1- (* a-low b-low)) p)) ;; ah*bl + bh*al + (al*bl-1 floor p) = 0 [p]
          (b-high (mod (* (- b-low) (+ (* a-high b-low) r)) q))))
   (+ (* b-high p) b-low)))
