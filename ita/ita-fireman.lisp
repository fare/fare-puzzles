;;;;; -*- common-lisp -*-
;;;;; This is the exact code I wrote during my interview,
;;;;; except for a then hardwired location for the dictionary file.
;;;;; The comments I added afterwards.

;;;; Firstly, constitute a hash table of the relevant words in the dictionary.
;;; Assume the dictionary is in the current directory.
(defvar *dictionary-file* "dictionary")

(defun read-dictionary ()
  (with-open-file (dict *dictionary-file* :direction :input)
    (loop for i = (read-line dict nil)
      while i collect i)))

(defun make-dict-hash ()
  (let ((dict (make-hash-table :test 'equal)))
    (loop for word in (read-dictionary)
      when (> (length word) 2)
      do (setf (gethash word dict) t))
    dict))

(defvar *dictionary* (make-dict-hash))

(defun in-dictionary-p (s)
  (gethash s *dictionary*))


;;;; Secondly, define the elementary search algorithm in CPS:
;;;; the elementary step of search is specified in continuation-passing style,
;;;; the continuation FOUND, taken as parameter, is called with appropriate
;;;; partial results.
(defun find-prefixes (w found &key start end)
  (loop for i from (1+ start) to end
    when (in-dictionary-p (subseq w start i))
    do (funcall found i)))

;;;; The full search is achieved by calling the search step with
;;;; a proper continuation that will do the rest of the job,
;;;; that is, recurse until a word is fully decomposed, and then call
;;;; *our* continuation (also named FOUND).
(defun do-word-decompositions (w found
			      &key (start 0) (end (length w)) (indices ()))
  (find-prefixes
   w
   #'(lambda (i)
       (let ((indices (cons i indices)))
	 (if (= i end)
	     (funcall found indices)
	   (do-word-decompositions
	    w found :start i :end end :indices indices))))
   :start start :end end))

;;;; A search of decompositions of a word that stores all its findings
(defun list-word-decompositions (w)
  (let ((list ()))
    (do-word-decompositions
     w
     #'(lambda (x) (push x list)))
    list))

;;;; A search of decompositions of a word that only remembers the best finding
(defun best-word-decomposition (w)
  (let ((best nil))
    (do-word-decompositions
     w
     #'(lambda (x)
	 (if (> (length x) (length best))
	     (setf best x))))
    best))

;;;; Now search for the best amongst the whole dictionary.
;;; Notice the bad pun with (length nil) being 0, for the initial challenge.
(defun find-best-word ()
  (let ((best nil))
    (maphash #'(lambda (word ignore)
		 (declare (ignore ignore))
		 (let ((decomposition (best-word-decomposition word)))
		   (if (>= (length decomposition) (length best))
		       (setf best (cons word decomposition)))))
	     *dictionary*)
    (format t "Best word: ~A (~A)~%" (car best) (cdr best))))


;;;; Performance was satisfying.
;;;; If we needed more, here would be possible optimizations.
;;;; In the context of only caring for the best result, we could
;;;; (1) break up words by length, and examine the longer words first
;;;; (2) abort partial searches in a given word that reach a letter
;;;; already reached using a previous (hence better) decomposition.
;;;; (3) abort partial searches that do not leave enough letters
;;;; to squeeze sufficient subwords to beat the contender.
;;;;
;;;; We could also easily have a list of ex-aequo winners, instead
;;;; of just the first one found so far.
