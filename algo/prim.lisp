;;; Prim's algorithm for minimum spanning tree
;;; https://en.wikipedia.org/wiki/Prim%27s_algorithm

(uiop:define-package :fare-puzzles/algo/prim
  (:use :common-lisp :uiop :cl-heap)
  (:import-from :cl-heap #:node-item)
  (:export
   #:read-weighted-undirected-graph-file
   #:minimum-spanning-tree))

(in-package :fare-puzzles/algo/prim)

(deftype graph ()
  "A weighted undirected graph is represented as a vector of length N+1, where
N is the number of vertices, the entry at index 0 contains the total weight of the graph,
and entry at index I contains a list of pairs (NEIGHBOR . WEIGHT) describing the edges
of the graph ending at vertex I."
  'vector)

(defun make-graph (number-of-vertices)
  (let ((g (make-array (list (1+ number-of-vertices)) :initial-element nil)))
    (setf (aref g 0) 0)
    g))

(defun graph-number-of-vertices (graph)
  (1- (length graph)))

(defun graph-vertex-edges (graph vertex)
  (aref graph vertex))

(defun add-edge! (graph vertex1 vertex2 weight)
  (declare (type graph graph))
  (let ((number-of-vertices (1- (length graph))))
    (assert (<= 1 vertex1 number-of-vertices))
    (assert (<= 1 vertex2 number-of-vertices)))
  (push (cons vertex2 weight) (aref graph vertex1))
  (push (cons vertex1 weight) (aref graph vertex2))
  (incf (aref graph 0) weight)
  t)

(defun string->integers (string)
  (mapcar #'parse-integer
	  (remove-if 'emptyp (split-string string :separator " "))))

(defun read-weighted-undirected-graph-file (pathname)
  "Read a file describing a weighted undirected graph, whereas:
The first line contains two integers, the number N of vertices (numbered from 1 to N),
and the number E of edges.
The subsequent lines describe edges as three integers, the two vertices being joined
and the weight of the edge.
Return an object of type GRAPH."
  (nest
   (with-input-file (i pathname))
   (destructuring-bind (number-of-vertices number-of-edges) (string->integers (read-line i)))
   (loop
     ;; Counting vertices from 1, the first entry of the array is empty
     :with graph = (make-graph number-of-vertices)
     :for line = (read-line i nil)
     :while line
     :sum 1 :into number-of-observed-edges
     :do (destructuring-bind (vertex1 vertex2 weight) (string->integers line)
	   (add-edge! graph vertex1 vertex2 weight))
     :finally)
   (progn
     (assert (= number-of-observed-edges number-of-edges))
     (return graph))))

(defun edge-weight< (edge1 edge2)
  "Compare two edges to a current vertex, where each edge is either NIL, representing
the absence of edge, which is superior to any other edge but itself, or a pair of
a vertex and a weight, and the superior edge is the one with the greater weight.
Return T if edge1 is strictly less than edge2 for this comparison."
  (if (and edge1 edge2)
      (< (cdr edge1) (cdr edge2))
      (and edge1 t)))

(defun vertex-edge-weight< (vew1 vew2)
  "Given two entries of vertex, neighbor . weight (v n . w) and/or only (v),
compare them by weight when defined (with infinite weight if no n and w),
otherwise compare them by vertex, then by neighbor.
Return T if vew1 is strictly less than vew2 for this comparison."
  (or (not (cdr vew2))
      (and (cdr vew1)
	   (or (edge-weight< (cdr vew1) (cdr vew2))
	       (and (= (cddr vew1) (cddr vew2))
		    (< (cadr vew1) (cadr vew2)))))))

(defun minimum-spanning-tree (graph)
  "Given a connected GRAPH in the same format as returned by READ-WEIGHTED-UNDIRECTED-GRAPH-FILE,
return a minimum spanning tree, which is a graph in the same format that happens to be
a tree, that contains all vertices in GRAPH, only edges in GRAPH, and has minimum weight among
graphs with the previous property. Use the Dijkstra-Jarnik-Prim greedy algorithm."
  (let* ((number-of-vertices ;; V
	  (graph-number-of-vertices graph))
	 (remaining-vertices ;; Q
	  (make-instance 'fibonacci-heap :sort-fun #'vertex-edge-weight<))
	 (vertex-candidate ;; C, E: associate to each vertex the proper node in the heap
	  (make-array (1+ number-of-vertices) :initial-element nil))
	 (spanning-forest ;; F
	  (make-graph number-of-vertices)))
    (loop
      :for v :from 1 :upto number-of-vertices
      :for node = (nth-value 1 (add-to-heap remaining-vertices (list v)))
      :do (setf (aref vertex-candidate v) node))
    (loop :until (is-empty-heap-p remaining-vertices)
      :do (destructuring-bind (vertex . edge) (pop-heap remaining-vertices)
	    (when edge
	      (destructuring-bind (neighbor . weight) edge
		(add-edge! spanning-forest vertex neighbor weight)))
	    (setf (aref vertex-candidate vertex) nil) ;; no longer in the candidate set.
	    (loop :for (neighbor . weight) :in (graph-vertex-edges graph vertex)
	      :for neighbor-candidate = (aref vertex-candidate neighbor)
	      :when neighbor-candidate :do
	      (let ((new-candidate `(,neighbor ,vertex . ,weight)))
		(when (vertex-edge-weight< new-candidate (node-item neighbor-candidate))
		  (decrease-key remaining-vertices neighbor-candidate new-candidate))))))
   spanning-forest))
