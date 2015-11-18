#|
https://projecteuler.net/problem=4
Largest Palindrome Product

A palindromic number reads the same both ways.
The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
|#

(uiop:define-package :fare-puzzles/euler/euler4
  (:use :uiop :cl
        :fare-puzzles/util/cache :fare-puzzles/util/extremizer
        :fare-puzzles/util/decimal :fare-puzzles/util/palindrome
        :fare-puzzles/util/simple-search :fare-puzzles/util/simple-integers)
  (:export #:solution #:euler-4))

(in-package :fare-puzzles/euler/euler4)

;;(declaim (optimize (speed 3) (safety 0) (debug 0)))
(declaim (optimize (speed 1) (safety 3) (debug 3)))


;;; OK, here's the shortest function that returns the solution:
(defun solution () 906609)

;;; Not what you were looking for?
;;; Want a general solution for n-digit numbers,
;;; that is fast enough to give the answer when n=3?
;;; OK.


;;; Here are the solutions to the problem, as computed so far by our best algorithms below.
(defcache *solutions* ()
  (0 0 0)
  (9 1 9)
  (9009 91 99)
  (906609 993 913)
  (99000099 9901 9999)
  (9966006699 99979 99681)
  (999000000999 999001 999999) ;; do you see a pattern for even values of n?
  (99901611610999 9991531 9998629)
  (9999000000009999 99990001 99999999)
  (999881760067188999 999882331 999999429)
  (99999834000043899999 9999986701 9999996699) ;; pattern broken already: it was only a minorant.
  (9999994020000204999999 99999943851 99999996349)
  (999999000000000000999999 999999000001 999999999999)
  (99999963342000024336999999 9999996340851 9999999993349)
  (9999999000000000000009999999 99999990000001 99999999999999)
  (999999974180040040081479999999 999999975838971 999999998341069)
  (99999999000000000000000099999999 9999999900000001 9999999999999999))

(defsequence (euler-4 *solutions*) (n)
  (euler-4.2 n))

;;; Now, brute force solution: enumerate all products, filter out those that are not palindromic.
(defun all-products-of-two-factors-below (n)
  (sort
   (remove-duplicates
    (while-collecting (c)
      (loop for i from 0 below n do
        (loop for j from 0 to i do
          (c (list (* i j) i j))))))
   '> :key 'first))

;;; Works well for n=3, doesn't work well for n=4
(defun euler-4.0 (n)
  "Biggest palindrome product of two factors below ten to the N"
  (loop for i in (all-products-of-two-factors-below (power-of-ten n))
        when (palindromep (first i)) return i))


;;; Same brute force as above, but supposing the top digit is 9,
;;; which as a low digit can be the product only of 1*9 or 3*3.
(defun candidate-factors-below-ten-to-the (n) ;;
  (while-collecting (c)
    (loop with p = (power-of-ten n)
          with first = (* 9/10 p)
          for m below (/ p 100) do
            (loop for last in '(1 3 9) do
              (c (+ first (* 10 m) last))))))

;;; This is already very slow for n=6, so we'll need a better heuristics to solve n=8
(defun euler-4.1 (n)
  (with-extremizer (c :test '> :key 'first)
    (loop for factors on (candidate-factors-below-ten-to-the n)
          for factor = (first factors) do
            (loop for other-factor in factors
                  for product = (* factor other-factor)
                  for candidate = (list product factor other-factor)
                  when (and (= 9 (mod product 10)) (palindromep product))
                    do (c candidate)))))


;;; OK. Let's generalize with several 9's as prefix, and do a bit of search,
;;; assuming the first (and last) m digits of the solution are 9's.
;;; That means the last digit of the factors will be 1, 3 or 9.

(defun euler-4.2 (n)
  (loop for m from (1- n) downto 0
        for x = (search-assuming-leading-nines n m)
        when x return x))

#|
Assume the product has m leading nines
(if n is even, we know m>=n/2; probably m>=(n-1)/2 for n odd, though I haven't proven it)
This in turn supposes that both factors have m leading nines.
Indeed, if a<10^n, b<(1-epsilon)*10^n, then ab<(1-epsilon)*10^2n, where epsilon=10^-m.
Thus, we only have to search for n-m digits.
If m>=1, we only the last digit of the factors will be 1, 3 or 9, and the other will be 9, 3 or 1,
so we only have to search 1 and 3 for the first factor.
If m>2, we can further refine that by searching only 1 or 13, 33, 53, 73, 93 for the first factor,
and the other factor will be of the other kind.
We know that m<n, because ab <= (10^n-1)^2 = 10^2n-2*10^n+1.


Q: can we also assume that one of the factors will have one more nine than m?
if a<(1-2*epsilon)*10^n, and b<(1-2*epsilon)*10^n, then ab<(1-8*epsilon+4*epsilon^2)*10^2n
not clear how that interferes with being a palindrome.

Can we similarly assume that n-m >= 2?
|#

(defun search-assuming-leading-nines (n m)
  (nest
   (case m
     ((0) (euler-4.0 n))
     ((1) (euler-4.1 n)))
   (otherwise)
   (with-extremizer (e :test '> :key 'first))
   (let* ((nn (+ n n))
          (pp (power-of-ten nn))))
   (flet ((c (a b)
            (let ((product (* a b)))
              ;;(DBG :c product a b)
              (when (palindromep product nn pp)
                (e (list product a b)))))))
   (let* ((end (power-of-ten n))
          (d (- n m))
          (freedom (power-of-ten d))
          (start (- end freedom))
          (p (power-of-ten m))
          (overlap (- (* 2 m) n)) ;; m= d_freedom+overlap
          (overlap-nines (if (plusp overlap) (1- (power-of-ten overlap)) 0))))
   (when (> d 2)) ;; otherwise, the problem is over-constrained.
   (loop for ah from start below end by 100 do)
   (loop for al in '(93 91 81 73 71 61 53 51 41 33 31 21 13 11 01) do)
   (let* ((a (+ ah al)) ;; first factor
          (a-low (mod a p))
          (b-low (- p (invert-modulo-10k m a-low))))) ;; top m are 9, low m are a-low
   ;;(progn (DBG :x end d freedom start p overlap overlap-nines ah al a a-low b-low))
   (if (plusp overlap)
         (when (= (floor b-low freedom) overlap-nines)
           (c a (+ start (mod b-low freedom)))))
   (loop for b from (+ start b-low) below end by p do)
   (c a b)))


;;; OK, now let's do a bit of finite domain constraint logic programming.
#|
The finite search space is the two sets of n digits for the two factors,
the set of 2*n digits for the product, and
the multiplication operations with carries that are intermediate results.
We build the constraint-propagation machine, which outputs a set of variables,
propagators for values, and optionally optimized propagators for sets of values.
Then we build a general set-maintenance system
for forward-constraint propagations with finite look-ahead.
Then we try to solve for the largest result.

If n is even n=2*m, then we have at least
  (* (1+ (* (1- (expt 10 m)) (expt 10 m)))
     (1- (expt 10 (* 2 m))))
  = (* (1- (expt 10 n)) (1+ (expt 10 n)))
e.g. for m=4:
         (* 99990001 99999999)

Suppose we have a better solution a' b' > a b
We know b' < b because for a better solution with b, the top m digits of the product would be 9,
thus the top bottom, at which point the bottom m digits of a must be 1, at which point a
is already the biggest solution.

|#


(defun euler-4.5 (n)
  n
  (error "Not implemented yet"))
