(import
  :gerbil/gambit/bytes
  :std/assert :std/iter
  :std/misc/hash :std/misc/list :std/misc/list-builder :std/misc/number :std/misc/ports :std/misc/string
  :std/sort :std/srfi/1 :std/srfi/13 :std/srfi/43 :std/sugar
  :clan/base :clan/basic-parsers :clan/config :clan/number
  :clan/path :clan/ports :clan/random :clan/source
  :clan/debug)

(export #t)

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

(def (least-element l (test <) (top +inf.0))
  (match l
    ([] top)
    ([a . d] (least-element d test (if (test a top) a top)))))

;; Shannon entropy contained in one case
(def (partial-information-content sub (total 1))
  (if (zero? sub) 0
      (let (p (/ sub total))
        (- (* p (log p 2))))))

;; Shannon entropy contained in one case and the opposite case
(def (information-content sub (total 1))
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

(def n-word 8636)
(def n-answers 238)
(def all-wordles (iota n-word))
(def word<-wi% (list->vector wordles))
(def wi<-word# (invert-hash<-vector word<-wi%))
(def (wi<-word w) (if (integer? w) w (hash-ref wi<-word# w)))
(def (word<-wi wi) (vector-ref word<-wi% wi))
(def answer<-ai% (make-vector n-answers (void)))
(def ai<-answer# (make-hash-table))
(def ai<-candidate-wordle%% (void))
(def (aicw-index candidate wordle) (+ (* candidate n-word) wordle))
(def (ai<-candidate-wordle candidate wordle)
  (u8vector-ref ai<-candidate-wordle%% (aicw-index candidate wordle)))
(def (ai<-candidate-wordle-set! candidate wordle val)
  (u8vector-set! ai<-candidate-wordle%% (aicw-index candidate wordle) val))
(def (ai<-answer a)
  (if (integer? a) a (hash-ref ai<-answer# a)))
(def (answer<-ai ai) (vector-ref answer<-ai% ai))

(def (precompute-wordle) ;; 53 seconds on my machine
  (assert! (= (vector-length word<-wi%) n-word))
  (def na 0)
  (set! ai<-candidate-wordle%% (make-u8vector (* n-word n-word) 255))
  (set! answer<-ai%
    (list->vector
     (with-list-builder (a)
       (def (intern-answer answer)
         (hash-ensure-ref ai<-answer# answer (lambda () (a answer) (post-increment! na))))
       (for (candidate all-wordles)
         (for (wordle all-wordles)
           (ai<-candidate-wordle-set!
            candidate wordle
            (intern-answer (wordle-answer (word<-wi candidate) (word<-wi wordle)))))))))
  (assert! (= na n-answers)))

(def wordle-cache (xdg-cache-home "fare-puzzles" "wordle.dat"))

(def (save-precomputed-wordle)
  (create-directory* (path-directory wordle-cache))
  (call-with-output-file wordle-cache
    (lambda (p)
      (for (ai (in-range n-answers))
        (write-bytes (string->bytes (answer<-ai ai)) p))
      (write-bytes ai<-candidate-wordle%% p))))

(def (load-precomputed-wordle)
  (call-with-input-file wordle-cache
    (lambda (p)
      (set! answer<-ai% (make-vector n-word (void)))
      (def buf (make-bytes 5 0))
      (for (ai (in-range n-answers))
        (read-bytes buf p)
        (vector-set! answer<-ai% ai (bytes->string buf)))
      (set! ai<-answer# (invert-hash<-vector answer<-ai%))
      (set! ai<-candidate-wordle%% (make-u8vector (* n-word n-word) 255))
      (read-bytes ai<-candidate-wordle%% p))))

(def (ensure-precomputed-wordle)
  (cond
   ((u8vector? ai<-candidate-wordle%%) (void))
   ((file-exists? wordle-cache) (load-precomputed-wordle))
   (else (precompute-wordle) (save-precomputed-wordle))))

;; Given a list of possible wordles, what is the entropy from the candidate?
(def (candidate-entropy candidate wordles)
  (def total n-word)
  (def buckets (make-vector n-answers 0))
  (for (wordle wordles)
    (def answer (ai<-candidate-wordle candidate wordle))
    (increment! (vector-ref buckets answer)))
  (for/fold (sum 0) (n buckets) (+ sum (partial-information-content n total))))

;; (Cons Real (List WI)) <- WI WI
(def (best-candidates candidates wordles)
  (def best '())
  (def score -inf.0)
  (for (candidate candidates)
    (def entropy (candidate-entropy candidate wordles))
    (cond
     ((> entropy score)
      (set! score entropy)
      (set! best [candidate]))
     ((= entropy score)
      (push! candidate best))))
  (cons score best))

;; Identify the best word given the constraints, with a heuristic considering
;; the information of each letter independently
(def (score-first-choices)
  (sort (map (lambda (candidate) [(answer<-ai candidate) (candidate-entropy candidate all-wordles)]) all-wordles)
        (comparing-key test: > key: second)))
;;==> in 86 seconds, I found that the best word is: "tares", entropy 6.22. Median word is "rodeo", entropy 4.65, average entropy 4.61, worst word "xylyl" entropy 2.16.

(def (thin-out-wordles candidate answer wordles)
  (filter (lambda (w) (equal? (ai<-candidate-wordle candidate w) answer)) wordles))
(def (best-candidate candidates wordles)
  (match (best-candidates candidates wordles)
    ([score . besties]
     (or (find (cut member <> wordles) besties)
         (first besties)))))

(def (play/i moves/i)
  (let loop ((wordles all-wordles) (ms moves/i))
    (match ms
      ([] (best-candidate all-wordles wordles))
      ([[candidate . answer] . more]
       (loop (thin-out-wordles candidate answer wordles) more)))))

(def (play . moves)
  (ensure-precomputed-wordle)
  (word<-wi (play/i (let loop ((ms moves))
                      (match ms ([] [])
                             ([candidate answer . more]
                              (cons (cons (wi<-word candidate) (ai<-answer answer))
                                    (loop more))))))))

;; Advice: always play "tares" first. For #222, found the answer in 4 steps:
;; (play "tares" "YBBBB" "colin" "BGBBY" "fount" "BGGGG") => ("mount" 0)
;; (best-candidate all-wordles all-wordles) ;=> (6.223902376629625 7422) in 3 seconds
(def best-first-play (wi<-word "tares"))
(def answer-found 0) ;; first entry is first word matching itself

(def (play-against wordle)
  (let loop ((wordles all-wordles) (candidate best-first-play) (n 1))
    (def answer (ai<-candidate-wordle candidate wordle))
    ;;(DBG play-against: candidate answer)
    (if (= answer answer-found)
      n
      (let (ws (thin-out-wordles candidate answer wordles))
        (loop ws (best-candidate all-wordles ws) (1+ n))))))

(def (all-plays)
  (ensure-precomputed-wordle)
  (for (w (shuffle-list all-wordles))
    (writeln [(word<-wi w) (play-against w)])))
