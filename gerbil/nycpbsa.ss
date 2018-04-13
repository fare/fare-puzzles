;; 2018-04-12
;; https://docs.google.com/forms/d/e/1FAIpQLSeLq83-NNjFBHDRdAmUoRhz9KUehH_LW-f13wbz5gHXz5obUw/viewform?fbzx=5826196183492550000
;; New York City Programming Blockchain Scholarship Application http://programmingblockchain.com/
;; Should you be selected, you will be given a free admission (value: $4000 USD) to the Programming Blockchain Seminar on May 17-18, 2018 in New York City. You will still be responsible for travel to and from the venue.
;; Please try to solve this problem, it's a requirement in order to be considered. How can you create a random 128-bit number from a deck of shuffled cards? Code a function deck_to_number() that takes a single argument, deck, a list of cards ("As", "Th", "7d", etc), that does this. Please do not use any library functions, especially SHA256 and the like. There should be a configuration of the deck that gets you to every possible 128-bit number.

(import
  :std/iter :std/misc/list :std/misc/shuffle :std/srfi/13 :std/sugar
  :clan/utils/assert)

(def suit-symbols #("♦diamonds" "♣clubs" "♥hearts" "♠spades"))
(def (suit-letter<-num num)
  (assert! (and (integer? num) (<= 0 num 3)))
  (string-ref (vector-ref suit-symbols num) 1))
(def (num<-suit-letter letter)
  (let/cc return
    (for (num (in-range 0 4))
      (when (equal? letter (suit-letter<-num num))
        (return num)))
    (error "not a suit letter" letter)))

(def card-letters "A23456789TJQK")
(def (card-letter<-num num)
  (assert! (and (integer? num) (<= 0 num 12)))
  (string-ref card-letters num))
(def (num<-card-letter letter)
  (let/cc return
    (for (num (in-range 0 13))
      (when (equal? letter (card-letter<-num num))
        (return num)))
    (error "not a card letter" letter)))

(def (cardsuit<-num num)
  (assert! (and (integer? num) (<= 0 num 51)))
  (list->string [(card-letter<-num (quotient num 4)) (suit-letter<-num (remainder num 4))]))

(def (num<-cardsuit cardsuit)
  (assert! (and (string? cardsuit) (= (string-length cardsuit) 2)))
  (let ((cardnum (num<-card-letter (string-ref cardsuit 0)))
        (suitnum (num<-suit-letter (string-ref cardsuit 1))))
    (+ (* 4 cardnum) suitnum)))

(def (identity-permutation n)
  (list->vector (with-list-builder (c) (for (i (in-range 0 n)) (c i)))))

(def (random-permutation n)
  (vector-shuffle (identity-permutation n)))

(def (num<-permutation p)
  (def n (vector-length p))
  ;; vector that maps the *remaining* numbers to the indices from 0 to n-1-k
  (def index<-value (identity-permutation n))
  ;; vector that maps the indices from 0 to n-1-k to the *remaining* numbers
  (def value<-index (identity-permutation n))
  (let loop ((k 0) (a 0))
    (if (>= k n)
      a
      (let* ((value (vector-ref p k)) ;; the k-th value in the permutation
             (i (vector-ref index<-value value)) ;; the index of that value in set of remaining values
             (m (- n k)) ;; the number of valid indices
             (l (- m 1)) ;; the last index
             (v (vector-ref value<-index l))) ;; value previously mapped to q
        (vector-set! index<-value v i)
        (vector-set! value<-index i v)
        (loop (+ k 1) (+ i (* m a)))))))

(def (fact n)
  (let loop ((p 1) (n n))
    (if (<= n 1) p (loop (* p n) (- n 1)))))

(def (permutation<-num num n)
  (def p (make-vector n))
  ;; vector that maps the *remaining* numbers to the indices from 0 to n-1-k
  (def index<-value (identity-permutation n))
  ;; vector that maps the indices from 0 to n-1-k to the *remaining* numbers
  (def value<-index (identity-permutation n))
  (let loop ((k 0) (f (fact (- n 1))) (a num))
    (when (< k n)
      (let* ((i (quotient a f))
             (a (remainder a f))
             (l (- n k 1)) ;; the last index
             (v (vector-ref value<-index l))) ;; value previously mapped to q
        (vector-set! p k (vector-ref value<-index i))
        (vector-set! index<-value v i)
        (vector-set! value<-index i v)
        (let ((k1 (+ k 1)))
          (when (< k1 n)
            (loop k1 (quotient f (- n k1)) a))))))
  p)

(def (deck<-permutation p)
  (vector-map cardsuit<-num p))

(def (random-deck)
  (deck<-permutation (random-permutation 52)))

(def (permutation<-deck deck)
  (vector-map num<-cardsuit deck))

(def (num<-deck deck)
  (num<-permutation (permutation<-deck deck)))

(def (byte128<-deck deck)
  (remainder (num<-deck deck) (expt 2 128)))
