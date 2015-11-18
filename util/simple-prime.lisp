;; Dealing with decimal representation of integers
(uiop:define-package :fare-puzzles/util/simple-prime
  (:use :uiop :cl
        :fare-puzzles/util/cache)
  (:export
   #:nth-small-prime
   #:small-prime-i
   #:small-prime-p
   #:primes-below
   #:erathostenes-sieve
   #:wheel-position
   ))

;; See also "The Genuine Sieve of Erathostenes" https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

(in-package :fare-puzzles/util/simple-prime)

;; NB: 0 is so the useful array indices start with 1
(defcache *small-primes* (:element-type 'integer) 0 2 3 5 7)
(defsequence (nth-small-prime *small-primes*) (n)
  (next-small-prime-above (nth-small-prime (1- n))))

;; sieve: 1 if the number is prime, 0 if composite
(defcache *prime-sieve* (:element-type 'bit) 0 0 1 1 0 1 0 1 0 0 0)

(defun next-small-prime-above (n)
  "Return the next prime number P such that P > N"
  (check-type n (integer 1 *))
  (loop for p from (1+ n) when (small-prime-p p) return p))

(defun small-prime-p (n)
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
  (aref *small-primes* (1- (fill-pointer *small-primes*))))

(defun compute-prime-wheel (primes)
  (let* ((product (reduce '* primes))
         (rp (coerce (loop for n from 0 below product collect (= 1 (gcd n product))) 'vector)))
    (check-type product (integer 2 *))
    (coerce (loop for n from 0 below product
                  collect (loop for i from (1+ n) when (aref rp (mod i product)) return (- i n)))
            'vector)))

(defparameter *wheel-2357* (coerce (compute-prime-wheel '(2 3 5 7)) '(vector (unsigned-byte 8))))

(defun wheel-position (wheel number)
  (declare (type vector wheel) (type integer number))
  (mod number (length wheel)))

(defun wheel-next (wheel number &optional (position (wheel-position wheel number)))
  (let ((increment (aref wheel position)))
    (declare (type fixnum position increment))
    (values (+ number increment) (mod (+ position increment) (length wheel)))))


(defun erathostenes-sieve (n)
  "Sieve of Erathostenes up to N"
  (let ((m (fill-pointer *prime-sieve*))
        (wheel *wheel-2357*))
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

;; pi function: numbers for prime not exceeding n
(defcache *small-pi* (:element-type 'integer) 0 0 1 2 2 3 3 4 4 4 4)
(defsequence (small-pi *small-pi*) (n)
  (let* ((m (fill-pointer *small-pi*))
         (sp (aref *small-pi* (1- m))))
    (adjust-array *small-pi* (1+ n))
    (setf (fill-pointer *small-pi*) (1+ n))
    (erathostenes-sieve n)
    (loop for m from m to n do
      (setf (aref *small-pi* m) (incf sp (aref *prime-sieve* m))))
    sp))
