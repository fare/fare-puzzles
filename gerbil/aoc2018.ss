;; Solutions to https://AdventOfCode.com/2018

(import
  :std/iter :std/misc/list :std/misc/repr :std/srfi/1 :std/srfi/43 :std/sugar
  :clan/utils/assert :clan/utils/base :clan/utils/basic-parsers
  :clan/utils/generator :clan/utils/hash :clan/utils/number :clan/utils/vector)

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
