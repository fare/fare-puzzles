(import
  :std/assert :std/iter
  :std/misc/hash :std/misc/number :std/misc/ports :std/misc/string
  :std/sort :std/srfi/1 :std/srfi/13 :std/srfi/43 :std/sugar
  :clan/base :clan/basic-parsers :clan/number :clan/path :clan/ports :clan/random :clan/source
  :clan/debug)

(def vector-ref-set! vector-set!)

(def wordles
  (!> "../data/enable1.txt"
      (cut subpath (this-source-directory) <>)
      read-file-lines
      (cut map string-trim-eol <>)
      (cut filter (lambda (x) (= (string-length x) 5)) <>)))

(def ascii-a (char->integer #\a))

(def letter? char-ascii-lowercase?)

(def (valid-word? word known-letters present-letters absent-letters)
  (let/cc return
    (for (i (in-range 5))
      (def wl (string-ref word i))
      (def kl (string-ref known-letters i))
      (when (or (and (letter? kl) (not (eqv? kl wl)))
                (string-index absent-letters wl))
        (return #f)))
    (for (pl present-letters)
      (unless (string-index word pl)
        (return #f)))
    #t))

(def (valid-words known-letters present-letters absent-letters)
  (filter (cut valid-word? <> known-letters present-letters absent-letters) wordles))

(def (index<-letter l)
  (def i (- (char->integer l) ascii-a))
  (assert! (<= 0 i 25))
  i)

(def (letter<-index i)
  (assert! (<= 0 i 25))
  (integer->char (+ i ascii-a)))

(def (letter-counts words)
  (def counts (make-vector 26))
  (for-each
    (lambda (w) (for-each (lambda (c) (increment! (vector-ref counts (index<-letter c))))
                     (delete-duplicates (string->list w))))
    words)
  counts)

(def (letters-by-count words)
  (!> (letter-counts words)
      (cut vector-map (lambda (i c) [(letter<-index i) c]) <>)
      vector->list
      ;;(lambda (x) (DBG foo: x))
      (cut filter (lambda (x) (plus? (second x))) <>)
      ;;(lambda (x) (DBG bar: x))
      (cut sort <> (comparing-key test: > key: second))))

(def (least-element l (test <) (top +inf.0))
  (match l
    ([] top)
    ([a . d] (least-element d test (if (test a top) a top)))))

(def (partial-information-content sub total)
  (if (zero? sub) 0
      (let (p (/ sub total))
        (- (* p (log p 2))))))

(def (information-content sub total)
  (+ (partial-information-content sub total)
     (partial-information-content (- total sub) total)))

;;; Compute directly word entropy: for each word,
;;; count the probabilities of all the possible colored results (Black Yellow Green),
;;; and see what entropy is achieved.
(def (wordle-answer candidate wordle)
  (def a (make-string 5 #\B))
  (def misplaced-count (make-vector 26 0))
  (for (i (in-range 5))
    (if (eqv? (string-ref candidate i) (string-ref wordle i))
      (string-set! a i #\G)
      (increment! (vector-ref misplaced-count (index<-letter (string-ref wordle i))))))
  (for (i (in-range 5))
    (unless (eqv? #\G (string-ref a i))
      (let (j (index<-letter (string-ref candidate i)))
        (when (plus? (vector-ref misplaced-count j))
          (string-set! a i #\Y)
          (decrement! (vector-ref misplaced-count j))))))
  ;;(DBG wordle-answer: candidate wordle a)
  a)

;; Given a list of possible wordles, what is the entropy from the candidate?
(def (candidate-entropy candidate wordles)
  (def total (length wordles))
  (def buckets (make-hash-table))
  (for (wordle wordles)
    (def answer (wordle-answer candidate wordle))
    (increment! (hash-ref buckets answer 0)))
  (reduce + 0 (map (cut partial-information-content <> total) (hash-values buckets))))

(def (candidates-by-entropy candidates wordles)
  (!> candidates
      (cut map (lambda (candidate) [candidate (candidate-entropy candidate wordles)]) <>)
      (cut sort <> (comparing-key test: > key: second))))

;; Identify the best word given the constraints, with a heuristic considering
;; the information of each letter independently
#;(writeln (candidates-by-entropy (take (shuffle-list words) 26) words))
(def (sort-wordles) (writeln (candidates-by-entropy wordles wordles)))
;;==> in 86 seconds, I found that the best word is: "tares", entropy 6.22. Median word is "rodeo", entropy 4.65, average entropy 4.61, worst word "xylyl" entropy 2.16.

(def (play . moves)
  (let loop ((ws wordles) (ms moves))
    (match ms
      ([] (let* ((candidates (candidates-by-entropy wordles ws))
                 (entropy (second (first candidates)))
                 (best-candidates (take-while (lambda (c) (= (second c) entropy)) candidates)))
            ;;(DBG play: (length ws) (length best-candidates))
            (or (find (lambda (c) (member (first c) ws)) best-candidates)
                (first best-candidates))))
      ([candidate answer . more]
       (loop (filter (lambda (w) (equal? answer (wordle-answer candidate w))) ws) more)))))

;; Advice: always play "tares" first. For #222, found the answer in 4 steps:
;; (play "tares" "YBBBB" "colin" "BGBBY" "fount" "BGGGG") => ("mount" 0)
