(import
  :gerbil/gambit/bytes
  :std/assert :std/iter
  :std/misc/hash :std/misc/list :std/misc/list-builder :std/misc/number :std/misc/ports :std/misc/string
  :std/sort :std/srfi/1 :std/srfi/13 :std/srfi/19 :std/srfi/43 :std/sugar
  :clan/assert :clan/base :clan/basic-parsers :clan/config :clan/number
  :clan/path :clan/ports :clan/random :clan/source :clan/timestamp
  :clan/debug)

;; See also: https://github.com/norvig/pytudes/blob/main/ipynb/Wordle.ipynb

(export #t)

(def vector-ref-set! vector-set!)

;; NB: This is the list of words from the file. Started on 2021-06-19 (local time).
;; List of 2309 words, until 2027-10-15.
(def wordles
  (!> "../data/wordles.txt"
      (cut subpath (this-source-directory) <>)
      read-file-lines
      (cut map string-trim-eol <>)
      #;(cut filter (lambda (x) (= (string-length x) 5)) <>))) ;; starts with the solutions, up to aahed

(def (first-wordle-date)
  (make-date 0 0 0 0 19 6 2021 0)) ;; TODO: should we adjust the final TZ field somehow?

(def n-solutions 2309) ;; number of wordle solutions
(defonce (solutions) (take wordles n-solutions))

(def (wordle-day)
  (quotient (- (current-unix-timestamp) (unix-timestamp<-date (first-wordle-date)))
            one-day))

(def (daily-wordle (day (wordle-day)))
  (list-ref wordles (modulo day n-solutions)))

(def ascii-a (char->integer #\a))

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

(def n-word 12947)
(def n-answers 238) ;; 3**5-5 < 256 (each result letter can be B, G or Y, but if there are 4 G's, the last one can't be Y)
(def all-wordles (iota n-word))
(def all-solutions (iota n-solutions))

;; Assign a natural number as word index to each word, starting from 0.
(def word<-wi% (list->vector wordles))
(def wi<-word# (invert-hash<-vector word<-wi%))
(def (wi<-word w) (hash-ref wi<-word# w))
(def (word<-wi wi) (vector-ref word<-wi% wi))

;; Assign a natural number as answer index to each answer, starting from 0.
;; Note that 0 will be for "GGGGG", the entry obtain by the the first word matching itself.
(def answer<-ai% (make-vector n-answers (void)))
(def ai<-answer# (make-hash-table))
(def (ai<-answer a) (hash-ref ai<-answer# a))
(def (answer<-ai ai) (vector-ref answer<-ai% ai))

;; Table from candidate index and solution index to answer index
(def ai<-candidate-wordle%% (void))
(def (aicw-index candidate wordle) (+ (* candidate n-word) wordle))
(def (ai<-candidate-wordle candidate wordle)
  (u8vector-ref ai<-candidate-wordle%% (aicw-index candidate wordle)))
(def (ai<-candidate-wordle-set! candidate wordle val)
  (u8vector-set! ai<-candidate-wordle%% (aicw-index candidate wordle) val))


(def (precompute-wordle) ;; 110 seconds on my machine
  (assert-equal! (vector-length word<-wi%) n-word)
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
  (assert-equal! na n-answers))
(def wordle-cache (xdg-cache-home "fare-puzzles" "wordle.dat"))

(def (save-precomputed-wordle) ;; 0.57s
  (create-directory* (path-directory wordle-cache))
  (call-with-output-file wordle-cache
    (lambda (p)
      (for (ai (in-range n-answers))
        (write-bytes (string->bytes (answer<-ai ai)) p))
      (write-bytes ai<-candidate-wordle%% p))))

(def (load-precomputed-wordle) ; 0.29s
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

;; : (Cons Real (List WI)) <- WI WI
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
  (sort (map (lambda (candidate) [(word<-wi candidate) (candidate-entropy candidate all-wordles)]) all-wordles)
        (comparing-key test: > key: second)))
;;==> in 5.25 seconds, I found that the best word is: "tares", entropy 6.19. Median word is "sibyl", entropy 4.63, average entropy 4.59, worst word "qajaq" entropy 2.07.

(def (thin-out-wordles candidate answer wordles)
  (filter (lambda (w) (equal? (ai<-candidate-wordle candidate w) answer)) wordles))
(def (best-candidate candidates wordles)
  (match (best-candidates candidates wordles)
    ([score . besties]
     (or (find (cut member <> wordles) besties)
         (first besties)))))

(def (play wordles: (wordles wordles) . moves)
  (ensure-precomputed-wordle)
  (let loop ((wordles (map wi<-word wordles)) (ms moves))
    (match ms
      ([candidate answer . more]
       (loop (thin-out-wordles (wi<-word candidate) (ai<-answer answer) wordles) more))
      ([]
       (let (c (best-candidate all-wordles wordles))
         (values (map word<-wi wordles) (length wordles) c (word<-wi c)))))))

;; Advice: always play "tares" first. For #222, found the answer in 4 steps:
;; (play "tares" "YBBBB" "colin" "BGBBY" "fount" "BGGGG") => "mount"
;; (best-candidate all-wordles all-wordles) ;=> 7422 in 3 seconds
(def best-first-play (wi<-word "tares"))

(defonce (first-play-buckets)
  (let (v (make-vector n-answers '()))
    (ensure-precomputed-wordle)
    (for (w all-wordles)
      (def a (ai<-candidate-wordle best-first-play w))
      (push! w (vector-ref v a)))
    v))

(def (play-against wordle)
  (if (= wordle best-first-play)
    1
    (let loop ((wordles
                (left-to-right vector-ref (first-play-buckets)
                               (ai<-candidate-wordle best-first-play wordle)))
               (n 2))
      (def candidate (best-candidate all-wordles wordles))
      (if (= wordle candidate)
        n
        (loop (thin-out-wordles candidate (ai<-candidate-wordle candidate wordle) wordles) (1+ n))))))

;; Explore the decision tree for all the relevant words in parallel.
;; Takes 504s (8min24s) on my machine.
;; Out of 12947 words, there are 23 7-step words: ferry folly casas coved doved fades fakes faxes fells finks gives hohed jacks jests jills safes sexes veges vexes vills vines wexes zills
(def (all-plays)
  (def solutions (make-vector n-word #f))
  (let process-bucket ((rpath []) (wordles all-wordles))
    #;(let* ((l (length wordles)) (w (if (<= l 10) wordles (take wordles 10))))
      (DBG process-bucket: rpath l w))
    (let (candidate (best-candidate all-wordles wordles))
      (let (bs (make-vector n-answers '()))
        (for (w wordles)
          (def a (ai<-candidate-wordle candidate w))
          (push! w (vector-ref bs a)))
        (for ((b bs) (a (in-naturals)))
          (def rp [[candidate . a] . rpath])
          (cond
           ((null? b) (void))
           ((null? (cdr b)) (set! (vector-ref solutions (car b))
                              (if (zero? a) [(length rp) . rp]
                                  [(1+ (length rp)) [(cdr b) . 0] . rp])))
           (else (process-bucket rp b)))))))
  solutions)
;; (def ap (time (all-plays)))
;; (let (c (make-vector 8 0)) (for ((a ap) (i (in-naturals))) (increment! (vector-ref c (car a)))) c)
;;=> #(0 1 58 2444 7248 2808 365 23)
;;
;; GOAL: change the decision tree to help with the worst case as opposed to the average case
;; Pick a better candidate by looking deeper down the tree?
;; Rotate some branches to rebalance the tree?
