;;; Prim's algorithm for minimum spanning tree
;;; https://en.wikipedia.org/wiki/Prim%27s_algorithm

(uiop:define-package :fare-puzzles/algo/prim
  (:use :common-lisp :uiop :cl-heap)
  (:import-from :cl-heap #:node-item)
  (:export
   #:read-weighted-undirected-graph-file
   #:minimum-spanning-forest))

(in-package :fare-puzzles/algo/prim)

(deftype graph ()
  "A weighted undirected graph is represented as a vector of length N+1, where
N is the number of vertices, the entry at index 0 contains the total weight of the graph,
and entry at index I contains a list of pairs (NEIGHBOR . WEIGHT) describing the edges
of the graph ending at vertex I."
  'simple-vector)

(defun make-graph (number-of-vertices)
  "Create a (mutable) graph with the given NUMBER-OF-VERTICES and no edges."
  (declare (type fixnum number-of-vertices))
  (let ((g (make-array (list (1+ number-of-vertices)) :initial-element nil)))
    (setf (svref g 0) 0)
    g))

(defun graph-number-of-vertices (graph)
  "Given a GRAPH, return the number of vertices"
  (declare (type graph graph))
  (1- (length graph)))

(defun graph-vertex-edges (graph vertex)
  "Given a GRAPH and a VERTEX, return a list of pairs (NEIGHBOR . WEIGHT)
describing the edges from that vertex in the graph"
  (svref graph vertex))

(defun add-edge! (graph vertex1 vertex2 weight)
  "Given a (mutable) GRAPH, two vertices VERTEX1 and VERTEX2 and a WEIGHT,
add to the GRAPH an edge with given weight linking given vertices."
  (declare (type graph graph) (type fixnum vertex1 vertex2) (type integer weight))
  (let ((number-of-vertices (1- (length graph))))
    (assert (<= 1 vertex1 number-of-vertices))
    (assert (<= 1 vertex2 number-of-vertices)))
  (push (cons vertex2 weight) (svref graph vertex1))
  (push (cons vertex1 weight) (svref graph vertex2))
  (incf (svref graph 0) weight)
  (values))

(defun string->integers (string)
  "Parse a string of space-delimited decimal integers as a list of integers"
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

(defun vertex-edge-weight< (vew1 vew2)
  "Given two entries of (vertex neighbor . weight) and/or only (vertex),
compare them by weight when defined (with infinite weight if no edge),
otherwise compare them by vertex, then by neighbor.
Return T if vew1 is strictly less than vew2 for this comparison."
  (nest
   (destructuring-bind (vertex1 . edge1) vew1)
   (destructuring-bind (vertex2 . edge2) vew2)
   (or (and (null edge1) (null edge2) (< vertex1 vertex2)))
   (and edge1)
   (or (null edge2))
   (destructuring-bind (neighbor1 . weight1) edge1)
   (destructuring-bind (neighbor2 . weight2) edge2)
   (or (< weight1 weight2))
   (and (= weight1 weight2))
   (or (< vertex1 vertex2))
   (and (= vertex1 vertex2))
   (< neighbor1 neighbor2)))

(defun minimum-spanning-forest (graph)
  "Given a GRAPH in the same format as returned by READ-WEIGHTED-UNDIRECTED-GRAPH-FILE,
return a minimum spanning forest, which is a graph in the same format that happens to be
forest, that contains all vertices in GRAPH, only edges in GRAPH, and has the same connected
components as the GRAPH, and has minimum weight among graphs with the previous property.
Use the Dijkstra-Jarnik-Prim greedy algorithm with a Fibonacci heap."
  (let* ((number-of-vertices ;; V -- one-letter names refer to variables in Wikipedia explanation.
	  (graph-number-of-vertices graph))
	 (remaining-vertices ;; Q
	  (make-instance 'fibonacci-heap :sort-fun #'vertex-edge-weight<))
	 (vertex-candidate ;; C, E: associate to each vertex the proper node in the heap
	  (make-array (1+ number-of-vertices) :initial-element nil)) ; count from 1
	 (spanning-forest ;; F
	  (make-graph number-of-vertices)))
    ;; Initialize Q, C, E: need to add all vertices to the forest, but none is connected yet.
    (loop
      :for v :from 1 :upto number-of-vertices
      :for node = (nth-value 1 (add-to-heap remaining-vertices (list v)))
      :do (setf (svref vertex-candidate v) node))
    ;; While there are more vertices, greedily pick the one with the lightest edge
    ;; (or else, the smallest remaining index), and add it to the forest, then
    ;; update its neighbors to reflect their distance to the forest.
    (loop :until (is-empty-heap-p remaining-vertices) :do
      (destructuring-bind (vertex . edge) (pop-heap remaining-vertices)
	;; TRACING: (format t "~D~@[ ~D ~D~]~%" vertex (car edge) (cdr edge))
	;; Adding the vertex to the forest == removing it from the candidate set.
	(setf (svref vertex-candidate vertex) nil)
	;; Add the connecting edge to the forest, if any
	(when edge
	  (destructuring-bind (neighbor . weight) edge
	    (add-edge! spanning-forest vertex neighbor weight)))
	;; For each neighbor that is still in the candidate set,
	;; update the priority queue if the new vertex makes it closer to the forest.
	(loop :for (neighbor . weight) :in (graph-vertex-edges graph vertex)
	  :for neighbor-candidate = (svref vertex-candidate neighbor)
	  :when neighbor-candidate :do
	  (let ((new-candidate `(,neighbor ,vertex . ,weight)))
	    (when (vertex-edge-weight< new-candidate (node-item neighbor-candidate))
	      (decrease-key remaining-vertices neighbor-candidate new-candidate))))))
    ;; Return the forest that now contains all vertices and a minimum set of edges.
    spanning-forest))
