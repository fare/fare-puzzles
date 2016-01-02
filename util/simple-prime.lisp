;; Dealing with small prime numbers (say up to 1e8, if using a few GiB of memory)
;; NB: This code maintains global tables and is generally not thread-safe.
;; See also "The Genuine Sieve of Erathostenes" https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

(uiop:define-package :fare-puzzles/util/simple-prime
  (:use :uiop :cl
        :fare-puzzles/util/cache)
  (:export
   #:nth-prime
   #:next-prime-above
   #:prime-p
   #:erathostenes-sieve
   #:pi-function
   #:divides-p
   #:factor
   ;; #:number-of-known-primes
   ;; #:largest-known-prime
   ;; #:compute-prime-wheel #:*wheel* #:wheel-position #:wheel-next
   ))

(in-package :fare-puzzles/util/simple-prime)

;;(declaim (optimize (speed 3) (safety 0) (debug 0))) ;; GO FAST!

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
  ;; Note that the these numbers MUST already be in *primes*, or else the sieve will fail.
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
(defcache *primes* (:element-type 'integer) 0 2 3 5 7)
(defsequence (nth-prime *primes*) (n)
  "Given an integer N, return the Nth prime number, starting with prime number 2 at index 1"
  (next-prime-above (nth-prime (1- n))))

;; sieve: 1 if the number is prime, 0 if composite
(defcache *prime-sieve* (:element-type 'bit) 0 0 1 1 0 1 0 1 0 0 0)

(defun next-prime-above (n)
  "Return the next prime number P such that P > N"
  (check-type n (integer 1 *))
  (loop with wp = (wheel-position *wheel* n)
        do (multiple-value-setq (n wp) (wheel-next *wheel* n))
        when (prime-p n) return n))

(defun prime-p (n)
  "Given a small integer N, is it a prime number? Answer using Erathostenes' sieve."
  (block nil
    (when (< n (fill-pointer *prime-sieve*))
      (return (plusp (aref *prime-sieve* n))))
    (loop for i from 1 below (fill-pointer *primes*)
          for p = (aref *primes* i) do
            (when (< n (* p p)) (return t))
            (when (zerop (mod n p)) (return nil))
          finally
             (let* ((r (isqrt n)))
               (erathostenes-sieve r)
               (return ;; not thread-safe:
                 (loop for i from (1+ i) below (fill-pointer *primes*)
                       never (zerop (mod n (aref *primes* i)))))))))

(defun largest-known-prime ()
  "The largest small prime computed so far"
  (aref *primes* (1- (fill-pointer *primes*))))

(defun erathostenes-sieve (n)
  "Run the sieve of Erathostenes up to N"
  (let ((m (fill-pointer *prime-sieve*))
        (wheel *wheel*))
    (when (>= n m)
      (let ((r (isqrt n)))
        (adjust-array *prime-sieve* (1+ n) :initial-element 1)
        (setf (fill-pointer *prime-sieve*) (1+ n))
        (loop for i from 1 below (fill-pointer *primes*)
              for p = (nth-prime i) do
                (when (> p r) (return))
                (loop with p2 = (* p p)
                      for q from (if (>= p2 m) p2 (* p (ceiling m p))) to n by p do
                  (setf (aref *prime-sieve* q) 0))
              finally
                 (loop with wp = (wheel-position wheel p) do
                   (multiple-value-setq (p wp) (wheel-next wheel p wp))
                   (when (> p n) (return))
                   (when (plusp (aref *prime-sieve* p))
                     (vector-push-extend p *primes*)
                     (loop with p2 = (* p p)
                           for q from (if (>= p2 m) p2 (* p (ceiling m p))) to n by p do
                             (setf (aref *prime-sieve* q) 0))))))
      (let ((p (largest-known-prime)))
        (when (< p n)
          (loop with wp = (wheel-position wheel p) do
                (multiple-value-setq (p wp) (wheel-next wheel p wp))
                (when (> p n) (return))
                (when (plusp (aref *prime-sieve* p))
                  (vector-push-extend p *primes*)))))))
  (make-array (1+ n) :displaced-to *prime-sieve* :element-type 'bit))


(defcache *pi* (:element-type 'integer) 0 0 1 2 2 3 3 4 4 4 4)
(defsequence (pi-function *pi*) (n)
  "pi(n) is the number of positive prime integers no greater than n"
  (let* ((m (fill-pointer *pi*))
         (sp (aref *pi* (1- m))))
    (adjust-array *pi* (1+ n))
    (setf (fill-pointer *pi*) (1+ n))
    (erathostenes-sieve n)
    (loop for m from m to n do
      (setf (aref *pi* m) (incf sp (aref *prime-sieve* m))))
    sp))

(defun divides-p (f n)
  "Does F divide N?"
  (check-type f (integer 0 *))
  (check-type n (integer 0 *))
  (if (zerop f)
      (zerop n)
      (zerop (mod n f))))

(defun factor (n)
  (check-type n (integer 1 *))
  (while-collecting (f)
    (loop
      with max
      for i from 1
      until (= 1 n) do
        (let ((prime (nth-prime i)))
          (unless max
            (setf max (isqrt n)))
          (when (> prime max)
            (f n) (return))
          (loop (multiple-value-bind (m r) (floor n prime)
                  (unless (zerop r) (return))
                  (setf n m max nil)
                  (f prime)))))))
