;; Dealing with small prime numbers (say up to 1e8, if using a few GiB of memory)
(uiop:define-package :fare-puzzles/util/simple-prime
  (:use :uiop :cl
        :fare-puzzles/util/cache)
  (:export
   #:nth-small-prime
   #:next-small-prime-above
   #:small-prime-p
   #:erathostenes-sieve
   #:small-pi
   ;; #:number-of-known-small-primes
   ;; #:largest-known-small-prime
   ;; #:compute-prime-wheel #:*wheel* #:wheel-position #:wheel-next
   ))

;; See also "The Genuine Sieve of Erathostenes" https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

(in-package :fare-puzzles/util/simple-prime)

(declaim (optimize (speed 3) (safety 0) (debug 0))) ;; GO FAST!


(defun compute-prime-wheel (primes)
  "Given a list of PRIMES, return a vector the size of which is the product M of those primes,
that at index I contains the smallest positive increment J such that I+J is a unit modulo M.
This makes it easier to skip over obvious composite numbers when looking for primes."
  (let* ((product (reduce '* primes))
         (rp (coerce (loop for n from 0 below product collect (= 1 (gcd n product))) 'vector)))
    (check-type product (integer 2 *))
    (coerce (loop for n from 0 below product
                  collect (loop for i from (1+ n) when (aref rp (mod i product)) return (- i n)))
            'vector)))

(defparameter *wheel*
  ;; Note that the these numbers MUST already be in *small-primes*, or else the sieve will fail.
  (coerce (compute-prime-wheel '(2 3 5 7)) '(vector (unsigned-byte 8)))
  "A wheel for skipping over numbers that are divisible by small primes. See COMPUTE-PRIME-WHEEL.")

(defun wheel-position (wheel number)
  "Compute the position of a NUMBER in the WHEEL. See COMPUTE-PRIME-WHEEL."
  (declare (type vector wheel) (type integer number))
  (mod number (length wheel)))

(defun wheel-next (wheel number &optional (position (wheel-position wheel number)))
  "Given a NUMBER at given POSITION in the WHEEL, return the next NUMBER that is not divisible
by a factor of the wheel size, and its position in the wheel."
  (let ((increment (aref wheel position)))
    (declare (type fixnum position increment))
    (values (+ number increment) (mod (+ position increment) (length wheel)))))


;; NB: 0 is so the useful array indices start with 1
(defcache *small-primes* (:element-type 'integer) 0 2 3 5 7)
(defsequence (nth-small-prime *small-primes*) (n)
  "Given an integer N, return the Nth prime number, starting with prime number 2 at index 1"
  (next-small-prime-above (nth-small-prime (1- n))))

;; sieve: 1 if the number is prime, 0 if composite
(defcache *prime-sieve* (:element-type 'bit) 0 0 1 1 0 1 0 1 0 0 0)

(defun next-small-prime-above (n)
  "Return the next prime number P such that P > N"
  (check-type n (integer 1 *))
  (loop with wp = (wheel-position *wheel* n)
        do (multiple-value-setq (n wp) (wheel-next *wheel* n))
        when (small-prime-p n) return n))

(defun small-prime-p (n)
  "Given a small integer N, is it a prime number? Answer using Erathostenes' sieve."
  (block nil
    (when (< n (fill-pointer *prime-sieve*))
      (return (plusp (aref *prime-sieve* n))))
    (loop for i from 1 below (fill-pointer *small-primes*)
          for p = (aref *small-primes* i) do
            (when (< n (* p p)) (return t))
            (when (zerop (mod n p)) (return nil))
          finally
             (let* ((r (floor (sqrt n))))
               (erathostenes-sieve r)
               (return ;; not thread-safe:
                 (loop for i from (1+ i) below (fill-pointer *small-primes*)
                       never (zerop (mod n (aref *small-primes* i)))))))))

(defun largest-known-small-prime ()
  "The largest small prime computed so far"
  (aref *small-primes* (1- (fill-pointer *small-primes*))))


(defun erathostenes-sieve (n)
  "Run the sieve of Erathostenes up to N"
  (let ((m (fill-pointer *prime-sieve*))
        (wheel *wheel*))
    (when (>= n m)
      (let ((r (floor (sqrt n))))
        (adjust-array *prime-sieve* (1+ n) :initial-element 1)
        (setf (fill-pointer *prime-sieve*) (1+ n))
        (loop for i from 1 below (fill-pointer *small-primes*)
              for p = (nth-small-prime i) do
                (when (> p r) (return))
                (loop with p2 = (* p p)
                      for q from (if (>= p2 m) p2 (* p (ceiling m p))) to n by p do
                  (setf (aref *prime-sieve* q) 0))
              finally
                 (loop with wp = (wheel-position wheel p) do
                   (multiple-value-setq (p wp) (wheel-next wheel p wp))
                   (when (> p n) (return))
                   (when (plusp (aref *prime-sieve* p))
                     (vector-push-extend p *small-primes*)
                     (loop with p2 = (* p p)
                           for q from (if (>= p2 m) p2 (* p (ceiling m p))) to n by p do
                             (setf (aref *prime-sieve* q) 0))))))
      (let ((p (largest-known-small-prime)))
        (when (< p n)
          (loop with wp = (wheel-position wheel p) do
                (multiple-value-setq (p wp) (wheel-next wheel p wp))
                (when (> p n) (return))
                (when (plusp (aref *prime-sieve* p))
                  (vector-push-extend p *small-primes*)))))))
  (make-array (1+ n) :displaced-to *prime-sieve* :element-type 'bit))


(defcache *small-pi* (:element-type 'integer) 0 0 1 2 2 3 3 4 4 4 4)
(defsequence (small-pi *small-pi*) (n)
  "pi(n) is the number of positive prime integers no greater than n"
  (let* ((m (fill-pointer *small-pi*))
         (sp (aref *small-pi* (1- m))))
    (adjust-array *small-pi* (1+ n))
    (setf (fill-pointer *small-pi*) (1+ n))
    (erathostenes-sieve n)
    (loop for m from m to n do
      (setf (aref *small-pi* m) (incf sp (aref *prime-sieve* m))))
    sp))
