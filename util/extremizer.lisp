(uiop:define-package :fare-puzzles/util/extremizer
  (:use :uiop :cl)
  (:export #:with-extremizer))

(in-package :fare-puzzles/util/extremizer)

;;; Since we're dealing with maximizing stuff, here is some function
;;; to find the greatest of considered candidates.
(defun call-with-extremizer (fun &key (test #'<) (key #'identity) default-value)
  (let ((has-candidate-p nil)
        (best-so-far nil)
        (best-key-so-far nil))
    (funcall fun
             (lambda (candidate)
               (let ((candidate-key (funcall key candidate)))
                 (when (or (not has-candidate-p)
                           (funcall test candidate-key best-key-so-far))
                   (setf has-candidate-p t
                         best-key-so-far candidate-key
                         best-so-far candidate)))))
    (if has-candidate-p
        best-so-far
        default-value)))

(defmacro with-extremizer ((c &rest keys &key test key default-value) &body body)
  "Compute BODY while function C is bound to a collector that remembers the best candidate,
where best is the smallest if TEST is <, or greatest if TEST is >, or otherwise the one such
that (PRED (KEY SOLUTION) (KEY OTHER-CANDIDATE)) for all candidates with different keys,
where PRED is a (possibly) strict total order relation. Return DEFAULT-VALUE if no candidate
was provided."
  (declare (ignore test key default-value))
  `(call-with-extremizer #'(lambda (,c) (flet ((,c (x) (funcall ,c x))) ,@body)) ,@keys))
