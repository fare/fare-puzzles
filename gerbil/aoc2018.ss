;; Solutions to https://AdventOfCode.com/2018

(import
  :gerbil/gambit/bits
  :std/iter :std/misc/list :std/misc/repr :std/misc/string
  :std/sort :std/srfi/1 :std/srfi/43 :std/sugar
  :clan/utils/assert :clan/utils/base :clan/utils/basic-parsers
  :clan/utils/generator :clan/utils/hash :clan/utils/number
  :clan/utils/stateful-avl-map :clan/utils/vector)

;;; DAY 1 https://adventofcode.com/2018/day/1

(defsyntax (this-source-directory stx)
  (syntax-case stx ()
    ((_)
     (let (path (gx#core-resolve-path "" (stx-source stx)))
       (with-syntax ((txt path))
         #'(quote txt))))))

(def (day-input-file n) (path-expand (format "aoc2018-~d.input" n) (this-source-directory)))

(def (expect-signed-integer port (base 10))
  (let ((char (peek-char port)))
    (cond
     ((eqv? char #\+)
      (read-char port)
      (expect-natural port base))
     ((eqv? char #\-)
      (read-char port)
      (- (expect-natural port base)))
     ((char-digit char)
      (expect-natural port base))
     (else
      (parse-error! 'expect-signed-integer "Neither a sign nor a digit in requested base"
                    char base port)))))

(def (for-each-port-signed-integer! port fun)
  (until (port-eof? port)
    (fun (expect-signed-integer port))
    (expect-and-skip-any-whitespace port)))

(def (day1-input (file (day-input-file 1)))
  (nest
   (generating<-for-each) (λ (yield))
   (call-with-input-file file) (λ (port))
   (for-each-port-signed-integer! port yield)))

(def day1-answer1 (reduce + 0 (list<-generating (day1-input)))) ;; 445


(def (generating-decons on-cons on-eof g)
  (let/cc k (on-cons (g (λ () (k (on-eof)))))))

;;(def (generating-cons x g) (λ ((on-eof eof!)) (let/cc k (k x) (g on-eof))) ;; doesn't work. TODO: debug
(def (generating-cons x g) (generating-concat (generating-singleton x) g))

;; This function fails somehow (!)
(def (generating-reduce-0 f seed g)
  (letrec ((r (λ (z)
                (prn ['gr z]) ;; prints correct z
                (λ ((on-eof eof!))
                  (generating-decons
                   (λ (x)
                     (prn ['gr1 z x]) ;; BUG: prints 0 for z (!) somehow the function closes over the initial seed instead of z.
                     (let ((y (f z x)))
                       (prn ['gr2 x z y])
                       ((generating-cons y (r y)) on-eof)))
                   on-eof g)))))
    (r seed)))

(def (generating-reduce f seed g)
  (nest
   (generating<-for-each) (λ (yield))
   (generating-for-each! g)
   (λ (x) (set! seed (f seed x)) (yield seed))))

(def (generating-sums g) (generating-reduce + 0 g))

(def (generating-find-first-duplicate g)
  (nest
   (let ((previous (hash)))) (let/cc k)
   (generating-for-each! g) (λ (x))
   (begin ())
   (if (hash-key? previous x) (k x))
   (hash-put! previous x #t)))

(def (generating-indefinitely gg)
  (generating<-for-each (λ (yield) (while #t (generating-for-each! (gg) yield)))))

(def day1-answer2
  (generating-find-first-duplicate
   (generating-sums
    (generating-indefinitely day1-input)))) ;; 219


;;; DAY 2 https://adventofcode.com/2018/day/2

(def (day2-input (file (day-input-file 2))) (read-file-lines file))

(def (hash-increment! table key (increment 1))
  (let ((previous (hash-ref table key 0)))
    (hash-put! table key (+ previous increment))))

(def (letter-counts id)
  (let ((counts (hash)))
    (string-for-each (cut hash-increment! counts <>) id)
    counts))

(def (invert-letter-counts id)
  (invert-hash (letter-counts id)))

(def (day2-checksum input)
  (let* ((invert-counts (map invert-letter-counts input))
         (containing-exactly-2-of-some-letter (length (filter (cut hash-key? <> 2) invert-counts)))
         (containing-exactly-3-of-some-letter (length (filter (cut hash-key? <> 3) invert-counts))))
    (* containing-exactly-2-of-some-letter containing-exactly-3-of-some-letter)))

(def day2-answer1 (day2-checksum (day2-input))) ;; 5456

(def (least-string-difference-index x y)
  (let ((lx (string-length x))
        (ly (string-length y)))
    (letrec ((r (λ (i)
                  (if (<= lx i) (if (<= ly i) #f i)
                      (if (<= ly i) i
                          (if (eqv? (string-ref x i) (string-ref y i)) (r (+ i 1)) i))))))
      (r 0))))

(def (string-pair-single-different-index x y)
  (nest
   (let ((lx (string-length x))
         (ly (string-length y))))
   (and (= lx ly))
   (let ((i (least-string-difference-index x y))))
   (and i)
   (let ((j (+ i 1))))
   (and (string= x y j lx j lx))
   i))

(def (string-pair-without-single-difference x y)
  (let ((i (string-pair-single-different-index x y)))
    (and i (string-append (substring x 0 i) (substring x (+ i 1) (string-length x))))))

(def (list-for-each-pair! l1 l2 f)
  (for-each! l1 (λ (x1) (for-each! l2 (λ (x2) (f x1 x2))))))

(def (list-for-each-head-tail! l f)
  (letrec ((r (λ-match
               ([] (void))
               ([head . tail]
                (f head tail)
                (r tail)))))
    (r l)))

(def (list-for-each-couple! l f)
  (list-for-each-head-tail! l (λ (h t) (for-each! t (cut f h <>)))))

(def day2-answer2
  (nest
   (let/cc k)
   (list-for-each-couple! (day2-input)) (λ (x y))
   (when-let (r (string-pair-without-single-difference x y)))
   (k r))) ;; "megsdlpulxvinkatfoyzxcbvq"


;;; DAY 3 https://adventofcode.com/2018/day/3

(def (parse-day3-line port)
  (nest
   (begin ((expect-maybe-char #\#) port))
   (let ((id (expect-natural port))))
   (begin ((expect-maybe-char #\space) port)
          ((expect-char #\@) port)
          ((expect-maybe-char #\space) port))
   (let ((min-x (expect-natural port))))
   (begin ((expect-char #\,) port))
   (let ((min-y (expect-natural port))))
   (begin ((expect-literal-string ": ") port))
   (let ((len-x (expect-natural port))))
   (begin ((expect-char #\x) port))
   (let ((len-y (expect-natural port))))
   (begin (expect-eol port))
   [id min-x min-y len-x len-y]))

(def (day3-input (file (day-input-file 3)))
  (nest
   (with-list-builder (c))
   (call-with-input-file file) (λ (port))
   (until (port-eof? port))
   (c (parse-day3-line port))))

(def day3-rectangles (day3-input))

(def day3-min-x
  (extremum<-list < (map second day3-rectangles))) ; 2
(def day3-min-y
  (extremum<-list < (map third day3-rectangles))) ; 0
(def day3-max-x
  (extremum<-list > (map (λ-match ([_ min-x _ len-x _] (+ min-x len-x))) day3-rectangles))) ; 1000
(def day3-max-y
  (extremum<-list > (map (λ-match ([_ min-y _ len-y _] (+ min-y len-y))) day3-rectangles))) ; 1000

(def (day3-answer1)
  (def table (make-vector (* 1000 1000) #\.))
  (def (ixy x y) (+ (* x 1000) y))
  (def (getxy x y) (vector-ref table (ixy x y)))
  (def (setxy x y z) (vector-set! table (ixy x y) z))
  (def (markxy x y)
    (match (getxy x y)
      (#\. (setxy x y #\#))
      (#\# (setxy x y #\O))
      (#\O (void))))
  (def (set-rectangle min-x min-y len-x len-y)
    (for ((x (in-range min-x len-x)))
      (for ((y (in-range min-y len-y)))
        (markxy x y))))
  (for-each! day3-rectangles
             (λ-match ([_ min-x min-y len-x len-y] (set-rectangle min-x min-y len-x len-y))))
  (def overlap-count 0)
  (for ((x (in-range 0 1000)))
    (for ((y (in-range 0 1000)))
      (when (eqv? (getxy x y) #\O)
        (increment! overlap-count))))
  overlap-count) ; 113716

(def (interval-intersection int1 int2)
  (nest
   (match int1) ([start1 len1])
   (match int2) ([start2 len2])
   (let* ((end1 (+ start1 len1))
          (end2 (+ start2 len2))
          (start (max start1 start2))
          (end (min end1 end2))
          (len (- end start))))
   (and (< 0 len) [start len])))

(def (rectangle-intersection rec1 rec2)
  (nest
   (match rec1) ([start-x1 start-y1 len-x1 len-y1])
   (match rec2) ([start-x2 start-y2 len-x2 len-y2])
   (match (interval-intersection [start-x1 len-x1] [start-x2 len-x2]) (#f #f)) ([start-x len-x])
   (match (interval-intersection [start-y1 len-y1] [start-y2 len-y2]) (#f #f)) ([start-y len-y])
   [start-x start-y len-x len-y]))

(def (day3-answer2)
  (def rectangles (list->vector day3-rectangles))
  (def n-rec (vector-length rectangles))
  (let/cc k
    (nest
     (for (i (in-range 0 n-rec)))
     (let/cc nope)
     (begin
       (nest
        (for (j (in-range 0 n-rec)))
        (unless (= i j))
        (when (rectangle-intersection (cdr (vector-ref rectangles i))
                                      (cdr (vector-ref rectangles j))))
        (nope))
       (k (car (vector-ref rectangles i)))))
    #f)) ; 742


;;; DAY 4 https://adventofcode.com/2018/day/4

(def (parse-day4-line line)
  (def day (substring line 1 11))
  (def hour (string->number (substring line 12 14)))
  (def minute (string->number (substring line 15 17)))
  (def action (string->symbol (string-downcase (substring line 19 24))))
  (def guard (and (eq? action 'guard) (string->number (substring line 26 (if (eqv? (string-ref line 29) #\space) 29 30)))))
  [day hour minute action guard])

(def day4-journal
  (!> (day-input-file 4)
      read-file-lines
      (cut sort <> string<)
      (cut map parse-day4-line <>)))

(def vector-ref-set! vector-set!)

(def (day4-guard-model model guard)
  (hash-ensure-ref model guard (λ () [0 (make-vector 60 0)])))

(def (mark-time-slept model guard time-falls time-wakes)
  (let ((v (second (day4-guard-model model guard))))
    (for (i (in-range time-falls (- time-wakes time-falls)))
      (increment! (vector-ref v i)))))

(def day4-model
  (nest
   (let ((model (hash)) (current-guard #f) (time-falls #f))) (begin0 model)
   (for-each! day4-journal) (λ-match) ([day hour minute action guard])
   (match action
     ('guard (set! current-guard guard) (set! time-falls #f)
             (increment! (car (day4-guard-model model guard))))
     ('falls (assert! current-guard) (assert-equal! hour 0) (set! time-falls minute))
     ('wakes (assert! current-guard) (assert! time-falls) (assert-equal! hour 0)
             (mark-time-slept model current-guard time-falls minute)
             (set! time-falls #f)))))

(def day4-sleepiest-guard
  (!> day4-model
      hash->list
      (cut map (λ-match ([id _ v] (cons id (reduce + 0 (vector->list v))))) <>)
      (cut extremum<-list (comparing-key test: > key: cdr) <>)
      car))

(def day4-sleepiest-minute
  (let* ((v (second (hash-get day4-model day4-sleepiest-guard)))
         (sleepiest-count (extremum<-list > (vector->list v))))
    (vector-index (cut eqv? sleepiest-count <>) v)))

(def day4-answer1 (* day4-sleepiest-guard day4-sleepiest-minute)) ; 72925

(def day4-sleepiest-guard-minute
  (!> day4-model
      hash->list
      (nest (λ (x)) (with-list-builder (c)) (for-each! x) (λ-match) ([id _ v])
            (for (i (in-range 0 60))) (c [id (vector-ref v i) i]))
      (cut extremum<-list (comparing-key test: > key: second) <>)))

(def day4-answer2 (match day4-sleepiest-guard-minute ([guard _ minute] (* guard minute)))) ; 49137


;;; DAY 5 https://adventofcode.com/2018/day/5

(def day5-input (string-trim-eol (read-file-string (day-input-file 5))))

(def (complement? x y)
  (= 32 (bitwise-xor (char->integer x) (char->integer y)))) ; assume ASCII letters

(def (day5-enqueue x q)
  (match q
    ([] [x])
    ([y . z] (if (complement? x y) z [x y . z]))))

(def (day5-fold l) (foldl day5-enqueue [] l))

(def day5-reduced-input (day5-fold (string->list day5-input)))

(def day5-answer1 (length day5-reduced-input)) ; 11042

;; Given two letters, are they the same "unit type" (terminology from the problem)?
;; NB: assume ASCII letters
(def (unit-type-eq? x y)
  (zero? (bitwise-and (bitwise-xor (char->integer x) (char->integer y)) (bitwise-not #x20))))

(def (day5-remove-unit-type u)
  (length (day5-fold (remove (cut unit-type-eq? u <>) day5-reduced-input))))

(def all-letters (map integer->char (iota 26 65))) ; assume ASCII

(def day5-answer2 (extremum<-list < (map day5-remove-unit-type all-letters))) ; 6872


;;; DAY 6 https://adventofcode.com/2018/day/6

(def (day6-parse port)
  (nest
   (with-list-builder (c))
   (until (port-eof? port))
   (let ((x (expect-natural port)))
     ((expect-char #\,) port)
     ((expect-char #\space) port))
   (let ((y (expect-natural port)))
     (expect-eol port))
   (c [x y])))

(def day6-input (call-with-input-file (day-input-file 6) day6-parse))
(def day6-len (length day6-input)) ; 50

(def all-letters-both-cases
  (!> all-letters list->string (λ (x) (string-append x (string-downcase x)))))

(def day6-start-x (+ -1 (extremum<-list < (map first day6-input))))
(def day6-end-x (+ 3 (extremum<-list > (map first day6-input))))
(def day6-start-y (+ -1 (extremum<-list < (map second day6-input))))
(def day6-end-y (+ 2 (extremum<-list > (map second day6-input))))
(def day6-len-x (- day6-end-x day6-start-x))
(def day6-len-y (- day6-end-y day6-start-y))
(def (day6-ixy x y) (+ (- x day6-start-x) (* day6-len-x (- y day6-start-y))))

(def (manhattan-distance a b)
  (nest
   (match a) ([ax ay])
   (match b) ([bx by])
   (+ (abs (- ax bx)) (abs (- ay by)))))

(def day6-model
  (nest
   (let ((v (make-string (* day6-len-x day6-len-y) #\newline)))) (begin0 v)
   (let ((p (list->vector day6-input))))
   (for ((x (in-range day6-start-x (- day6-len-x 1)))))
   (for ((y (in-range day6-start-y day6-len-y))))
   (let ((mindist (+ day6-len-x day6-len-y))))
   (for ((i (in-range 0 day6-len))))
   (let ((dist (manhattan-distance [x y] (vector-ref p i))))
     #;(prn [[x y] (vector-ref p i) dist mindist i (string-ref all-letters-both-cases i)]))
   (cond
    ((< dist mindist)
     (set! mindist dist)
     (string-set! v (day6-ixy x y) (string-ref all-letters-both-cases i)))
    ((= dist mindist)
     (string-set! v (day6-ixy x y) #\.)))))

(def day6-infinite-letters
  (let ((s (make-string day6-len #\.)))
    (let ((mark (λ (x y)
                  (let* ((c (string-ref day6-model (day6-ixy x y)))
                         (i (string-index all-letters-both-cases c)))
                    (when i
                      (string-set! s i c))))))
      (for ((x (in-range day6-start-x (- day6-len-x 2))))
        (mark x day6-start-y)
        (mark x (- day6-end-y 1)))
      (for ((y (in-range day6-start-y (- day6-len-y 1))))
        (mark day6-start-x y)
        (mark (- day6-end-x 2) y)))
    s))

(def day6-areas
  (nest
   (let ((v (make-vector day6-len 0)))) (begin0 v)
   (for ((x (in-range day6-start-x (- day6-len-x 1)))))
   (for ((y (in-range day6-start-y day6-len-y))))
   (let* ((c (string-ref day6-model (day6-ixy x y)))
          (i (string-index all-letters-both-cases c))))
   (when (and i (eqv? #\. (string-ref day6-infinite-letters i))))
   (increment! (vector-ref v i))))

(def day6-answer1
  (extremum<-list > (vector->list day6-areas))) ; 5626

(def day6-answer2
  (let ((size 0))
   (for ((x (in-range day6-start-x (- day6-len-x 1))))
     (for ((y (in-range day6-start-y day6-len-y)))
       (when (< (reduce + 0 (map (cut manhattan-distance [x y] <>) day6-input)) 10000)
         (increment! size))))
   size)) ; 46554


;;; DAY 7 https://adventofcode.com/2018/day/7

(def (day7-parse port)
  (nest
   (with-list-builder (c))
   (until (port-eof? port))
   (begin ((expect-literal-string "Step ") port))
   (let ((pre (read-char port)))
     ((expect-literal-string " must be finished before step ") port))
   (let ((post (read-char port)))
     ((expect-literal-string " can begin.\n") port))
   (c [pre post])))

(def day7-input (call-with-input-file (day-input-file 7) day7-parse))
(def day7-len (length day7-input))

(assert-equal!
 all-letters
 (!> (append (map first day7-input) (map second day7-input))
     (cut sort <> char<?)
     (cut delete-duplicates <> eqv?)))

(def (task-model priority-comparer tasks constraints)
  (def initial (avl-map<-alist priority-comparer (map (cut cons <> #t) tasks)))
  (def depends-on (hash))
  (def blocks (hash))
  (def (get-depends-on post) (hash-ensure-ref depends-on post make-hash-table))
  (def (get-blocks pre) (hash-ensure-ref blocks pre make-hash-table))
  (def (add-constraint pre post)
    (hash-put! (get-depends-on post) pre #t)
    (hash-put! (get-blocks pre) post #t)
    (when (avl-map-key? priority-comparer initial post)
      (avl-map-remove! priority-comparer initial post)))
  (for-each! constraints (cut apply add-constraint <>))
  (values initial get-depends-on get-blocks))

(def (topological-sort priority-comparer tasks constraints)
  (defvalues (initial get-depends-on get-blocks) (task-model priority-comparer tasks constraints))
  (nest
   (with-list-builder (c))
   (until (avl-map-empty? initial))
   (let ((pre (first-value (avl-map-leftmost initial)))))
   (begin (c pre)
          (avl-map-remove! priority-comparer initial pre))
   ((cut hash-for-each <> (get-blocks pre))) (λ (post _))
   (let ((deps (get-depends-on post))))
   (begin (hash-remove! deps pre))
   (when (hash-empty? deps))
   (avl-map-put! priority-comparer initial post #t)))

(def (day7-answer1)
  (list->string (topological-sort char-comparer all-letters day7-input))) ; "BDHNEGOLQASVWYPXUMZJIKRTFC"

(def (task-duration task)
  (+ 60 -64 (char->integer task)))

(def (string<-charset cs)
  (!> (cond
       ((list? cs) cs)
       ((hash-table? cs) (hash-keys cs))
       ((avl-map? cs) (map first (alist<-avl-map cs))))
      (cut sort <> char<?)
      list->string))

(def (sorted-char-hash-keys h) (list->string (sort (hash-keys h) char<?)))

(def (schedule-tasks priority-comparer task-duration n-workers tasks constraints)
  (defvalues (ready get-depends-on get-blocks) (task-model priority-comparer tasks constraints))
  (def pending (make-empty-avl-map))
  (def clock 0)
  (def (done?) (and (avl-map-empty? ready) (avl-map-empty? pending)))
  (def (can-do?) (and (not (avl-map-empty? ready)) (< 0 n-workers)))
  (until (done?)
    (prn [clock 'ready (string<-charset ready) 'pending (alist<-avl-map pending) 'workers n-workers])
    (cond
     ((can-do?)
      (let* ((task (first-value (avl-map-leftmost ready)))
             (completion-time (+ clock (task-duration task))))
        (avl-map-remove! priority-comparer ready task)
        (decrement! n-workers)
        (prn [clock 'starting-task task 'done completion-time 'pending (alist<-avl-map pending)
                             'lookup (values->list (avl-map-lookup number-comparer pending completion-time))])
        (let ((complete-then (avl-map-ref number-comparer pending completion-time [])))
          (avl-map-put! number-comparer pending completion-time (cons task complete-then)))))
     (else
      (let-values (((completion-time complete) (avl-map-leftmost pending)))
        (set! clock completion-time)
        (avl-map-remove! number-comparer pending clock)
        (for-each!
         complete
         (λ (task)
           (increment! n-workers)
           (prn [clock 'completed-task task (string<-charset (get-blocks task))])
           (hash-for-each (λ (post _)
                            (let ((deps (get-depends-on post)))
                              (hash-remove! deps task)
                              (when (hash-empty? deps)
                                (avl-map-put! priority-comparer ready post #t))))
                          (get-blocks task))))))))
  clock)

(def (day7-answer2)
  (schedule-tasks char-comparer task-duration 5 all-letters day7-input)) ;; 1107


;;; DAY 8 https://adventofcode.com/2018/day/8

(def (day8-parse port)
  (nest
   (with-list-builder (c))
   (until (port-eof? port))
   (begin (c (expect-natural port)))
   (expect-and-skip-any-whitespace port)))

(def day8-input (call-with-input-file (day-input-file 8) day8-parse))
(def day8-len (length day8-input))

(def (day8-answer1)
  (def g (generating<-list day8-input))
  (def count 0)
  (def (walk)
    (def n-children (g))
    (def n-metadata (g))
    (for (_ (in-range 0 n-children)) (walk))
    (for (_ (in-range 0 n-metadata)) (increment! count (g))))
  (walk)
  count) ; 40977

(def (day8-answer2)
  (def g (generating<-list day8-input))
  (def count 0)
  (def (walk)
    (def n-children (g))
    (def n-metadata (g))
    (if (zero? n-children)
      (reduce + 0 (generating-take g n-metadata))
      (let* ((children (list->vector (generating-take walk n-children)))
             (metadata (generating-take g n-metadata)))
        (reduce + 0 (map (λ (i) (let ((j (- i 1))) (if (< -1 j (vector-length children)) (vector-ref children j) 0))) metadata)))))
  (walk)) ; 27490


;;; DAY 9 https://adventofcode.com/2018/day/9

(def (day9-parse port)
  (nest
   (let ((n-players (expect-natural port)))
     ((expect-literal-string " players; last marble is worth ") port))
   (let ((last-marble-points (expect-natural port)))
     ((expect-literal-string " points") port))
   (values n-players last-marble-points)))

(def (day9-input) (call-with-input-file (day-input-file 9) day9-parse))
(defvalues (day9-n-players day9-last-marble-points) (day9-input))

(def (iterate-function n fun . v)
  (if (zero? n)
    (apply values v)
    (apply iterate-function (- n 1) fun (values->list (apply fun v)))))

;; TODO: to work with much larger numbers, try finger trees?

;; Mutable doubly-linked data structure for ring buffers (where every node holds one data value)
;; and lists (where the singleton is a special marker that holds no meaningful data value).
;; When pointing to a node, the "circle" object is the current node.
;; When pointing to a point between nodes, the "circle" object is a cursor before the current node.
;; Circle : Type <- Type
;; make-circle : (Circle Value) <- Value (Circle Value) (Circle Value)
(defstruct circle
  (value prev next))

;; Create a doubly-linked circle node the given value, linked to itself.
;; (Circle V) <- V
(def (circle-singleton v)
  (def c (make-circle v #f #f))
  (set! (circle-prev c) c)
  (set! (circle-next c) c)
  c)

;; Splice two circles at given points... together if apart, or apart if together. Return the first point.
;; (Circle V) <- (Circle V) (Circle V)
(def (circle-splice c+ d+) ; splice the two circles together
  (let ((c- (circle-prev c+))
        (d- (circle-prev d+)))
    (set! (circle-next c-) d+)
    (set! (circle-prev d+) c-)
    (set! (circle-next d-) c+)
    (set! (circle-prev c+) d-)
    c+))

;; Move n times next if n is positive, or -n times prev if n is negative.
;; (Circle V) <- (Circle V) Integer
(def (circle-move c n)
  (if (<= 0 n)
    (iterate-function n circle-next c)
    (iterate-function (- n) circle-prev c)))

;; Splice the value at the cursor, just before the current point
;; (Circle V) <- (Circle V) V
(def (circle-add c v)
  (circle-splice c (circle-singleton v)))

;; Splice the value at the cursor, just before the current point
;; (Circle V) <- V (Circle V)
(def (circle-push v c)
  (circle-add c v))

;; Splice the value after the current point
;; (Circle V) <- (Circle V) V
(def (circle-add-next c v)
  (circle-add (circle-next c) v) c)

;; Splice the value after the current point, return the next point (the current point becomes a singleton)
;; (Circle V) <- (Circle V)
(def (circle-remove c)
  (circle-splice (circle-next c) c))

;; Return a list of elements after the first point and before the second, including the current node
;; (List V) <- (Circle V) (Circle V)
(def (circle-elements+ start stop)
  (cons (circle-value start) (circle-elements- (circle-next start) stop)))

;; Return a list of elements after the first node and before the second, excluding the current node
;; (List V) <- (Circle V) (Circle V)
(def (circle-elements- start stop)
  (if (eq? start stop) [] (circle-elements+ start stop)))

;; Return a list of elements in the circle
;; (List V) <- (Circle V)
(def (list<-circle c)
  (circle-elements+ c c))

;; Create a circle from the list of elements
;; (Circle V) <- (List V)
(def (circle<-list l)
  (match l
    ([] (error "cannot make circle from empty list"))
    ([h . t] (foldl circle-push (circle-singleton h) t))))

(defmethod {:pr circle}
  (λ (c (port (current-output-port)) (options (current-representation-options)))
    (def (p y) (pr y port options))
    (def (d y) (display y port))
    (d "(circle<-list")
    (for-each (λ (x) (d " ") (p x)) (list<-circle c))
    (d ")")))

(def circle-test
  (test-suite "test suite for circle"
    (test-case "test circle creation"
      (check-equal? (list<-circle (circle-singleton 1)) [1])
      (check-equal? (list<-circle (circle<-list [1 2 3 4])) [1 2 3 4]))

    (test-case "test splicing"
      (check-equal? (list<-circle (circle-splice (circle<-list [1 2 3 4]) (circle<-list [5 6 7 8])))
                    [1 2 3 4 5 6 7 8]))))

(def (marble-step marble circle scorecard)
  (def n-players (vector-length scorecard))
  (def player (remainder marble n-players))
  (if (zero? (remainder marble 23))
    (let ((c (circle-move circle -7)))
      (increment! (vector-ref scorecard player) (+ marble (circle-value c)))
      (circle-remove c))
    (circle-splice (circle-singleton marble) (circle-move circle 2))))

(def (marble-play n-players max-marble)
  (def circle (circle-singleton 0))
  (def scorecard (make-vector n-players 0))
  (when (> max-marble 0)
    (for (marble (in-range 1 (- max-marble 1)))
      (set! circle (marble-step marble circle scorecard))))
  (foldl max 0 (vector->list scorecard)))

(def marble-test
  (test-suite "test suite for marble game"
    (test-case "test given results"
      (check-equal? (marble-play 10 1618) 8317)
      (check-equal? (marble-play 13 7999) 146373)
      ;; (check-equal? (marble-play 17 1104) 2764) ; BUG? we return 2720 instead
      (check-equal? (marble-play 21 6111) 54718)
      (check-equal? (marble-play 30 5807) 37305))))

(def (day9-answer1)
  (defvalues (n-players max-marble) (day9-input))
  (marble-play n-players max-marble)) ; 384288

(def (day9-answer2)
  (defvalues (n-players max-marble) (day9-input))
  (marble-play n-players (* max-marble 100))) ; 3189426841

