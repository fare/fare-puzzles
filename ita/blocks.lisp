#!/usr/bin/cl-launch -X --
#|
You may run this program with:
	cl-launch -f blocks.lisp
(Run it twice: the first time around, it may spew compilation notes.)
|#

#|
Puzzle from Athena Health

Your nephew received a set of blocks for his birthday, and he has decided
to build a panel out of rows of 3"×1" and 4.5"×1" blocks. For structural
integrity, he does not want the spaces between the blocks to be aligned in
adjacent rows. As an example, he would not accept the 13.5" ×3" panel
shown below, because the spaces between the blocks in the top two rows
line up (as shown by the X-ed line).

___________________
|_____|___|___X___|
|___|___|_____X___|
|_____|_____|_____|

Thus, there are 2 ways to build a 7.5"×1" panel, 2 ways to build a
7.5"×2" panel, 4 ways to build a 12"×3" panel, and 7958 ways to build a
27"×5" panel.

In how many different ways could your nephew build a 48"×10" panel? A
64-bit signed integer is large enough to hold the answer. Write a program
to calculate the answer. Your program will be judged on how fast it runs
and how concise the source code is (while still being readable). On a 2.5
GHz Xeon, without pre-computing any values, our C solution runs in 20 ms
and our Perl solution runs in 850 ms, Let us know the value that your
program computes, your program’s running time, and the kind of machine on
which you ran it. Attach your source code.
|#


;; For debugging:
(declaim (optimize (speed 1) (safety 3) (debug 3)))

;; For speed: (disable by prepending #+(or) or commenting in #| ... |# )
(eval-when (:compile-toplevel :load-toplevel)
  (declaim (optimize (speed 3) (safety 0) (debug 0)))
  (proclaim '(optimize (speed 3) (safety 0) (debug 0)))
  (pushnew :unsafe *features*)) ;; assumes a 64-bit CPU for large enough fixnums

#+unsafe
(progn
  (declaim (inline f-)) ;; f- is a "fast" version of -
  (defun f- (x y)
    (the fixnum (- (the fixnum x) (the fixnum y)))))
#-unsafe
(progn
  (defmacro f- (x y)
    `(- ,x ,y)))


(defun call-with-collector (generator)
  (let ((accumulator ()))
    (funcall generator (lambda (x) (push x accumulator)))
    (nreverse accumulator)))

(defmacro with-collector ((collector-var) &body body)
  `(call-with-collector (lambda (,collector-var) ,@body)))

(defun generate-rows-completing (collector remaining-length current-list)
  (cond
    ((zerop remaining-length)
     (funcall collector current-list))
    (t
     (when (<= 2 remaining-length)
       (generate-rows-completing collector (f- remaining-length 2) (cons 2 current-list))
       (when (<= 3 remaining-length)
         (generate-rows-completing collector (f- remaining-length 3) (cons 3 current-list)))))))

(defun generate-rows (n)
  (coerce
   (with-collector (c)
     (generate-rows-completing c n ()))
   'vector))

#|
(defun rows-compatible-from-p (r1 l1 r2 l2)
  (or (zerop l1)
      (zerop l2)
      (cond
        ((> l1 l2)
         (rows-compatible-from-p (cdr r1) (f- l1 (car r1)) r2 l2))
        ((< l1 l2)
         (rows-compatible-from-p r1 l1 (cdr r2) (f- l2 (car r2))))
        (t
         nil))))

(defun rows-compatible-p (r1 r2 length)
  (or (zerop length)
      (rows-compatible-from-p (cdr r1) (f- length (car r1)) r2 length)))
|#

;; Optimized version
(defun rows-compatible-p (r1 r2 length)
  (prog ((l1 length) (l2 length))
     (declare (type fixnum l1 l2))
     :l1-shorter* ;; also true initially
     (when (zerop l1)
       (return t))
     :l2-longer*
     (setf l2 (f- l2 (car r2))
           r2 (cdr r2))
     (cond
       ((< l1 l2) (go :l2-longer*))
       ((> l1 l2) (rotatef l1 l2) (rotatef r1 r2) (go :l1-shorter*))
       (t (return nil)))))

(defun compute-row-compatibility (rows length)
  (check-type length (and fixnum unsigned-byte))
  (check-type rows simple-vector)
  (loop :with size = (length rows)
        :with matrix = (make-array (list size size)
                                   :element-type t :initial-element nil)
        :for i fixnum :below size :do
        (loop :for j fixnum :from (1+ i) :below size
              :when (rows-compatible-p (aref rows i) (aref rows j) length)
              :do (setf (aref matrix i j) t
                        (aref matrix j i) t))
        :finally (return matrix)))

;;For debugging
(defun list-compatible-rows (length)
  (check-type length unsigned-byte)
  (let* ((rows (generate-rows length))
         (size (length rows))
         (compatibility-matrix (compute-row-compatibility rows length)))
    (loop :for i :below size :do
          (loop :for j :from (1+ i) :below size
                :when (aref compatibility-matrix i j)
                :do (format t "~&~S + ~S~%" (aref rows i) (aref rows j))))))

;;For debugging
(defun list-row-combinations (length height)
  (check-type length unsigned-byte)
  (check-type height unsigned-byte)
  (if (zerop height) '(())
      (let* ((rows (generate-rows length))
             (size (length rows))
             (compatibility-matrix (compute-row-compatibility rows length))
             (combinations-matrix (make-array (list size height)
                                              :initial-element nil)))
        (loop :for i :below size :do
              (setf (aref combinations-matrix i 0) (list (list (aref rows i)))))
        (loop :for h :from 1 :below height :do
              (loop :for i :below size :do
                    (setf (aref combinations-matrix i h)
                          (loop :for j :below size
                                :when (aref compatibility-matrix i j)
                                :append
                                (mapcar #'(lambda (x)
                                            (cons (aref rows i) x))
                                        (aref combinations-matrix j (1- h)))))))
        (loop :for i :below size
              :append (aref combinations-matrix i (1- height))))))

(defun count-row-combinations (length height)
  (check-type length fixnum)
  (check-type height fixnum)
  (if (zerop height) 1
      (let* ((rows (the simple-vector (generate-rows length)))
             (size (the fixnum (length (the simple-vector rows))))
             (compatibility-matrix (the simple-array
                                     (compute-row-compatibility rows length)))
             (combinations-matrix (make-array (list size height)
                                              :element-type
                                              #+unsafe 'fixnum
                                              #-unsafe 'unsigned-byte)))
        (loop :for i fixnum :below size :do
              (setf (aref combinations-matrix i 0) 1))
        (loop :for h fixnum :from 1 :below height :do
              (loop :for i fixnum :below size :do
                    (setf (aref combinations-matrix i h)
                          (the fixnum
                            (loop :for j fixnum :below size
                                  :when (aref compatibility-matrix i j)
                                  :sum (aref combinations-matrix j (1- h)))))))
        (loop :for i fixnum :below size
              :sum (aref combinations-matrix i (1- height))))))

(defun print-combination-results (length height)
  (format t "~&There are ~A ways to build a ~F\"x~D\" panel.~%"
          (count-row-combinations length height)
          (* 3/2 length)
          height))

(defun check-known-results ()
  (loop :for (length height) :in '((5 1) (5 2) (8 3) (18 5))
        :do (print-combination-results length height)))

(defun main ()
  ;;(check-known-results)
  ;;(list-compatible-rows 8)
  ;;(list-row-combinations 8 2)
  (print-combination-results 32 10))

(time (main))
