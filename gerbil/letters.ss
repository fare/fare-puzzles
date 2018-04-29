package: puzzles

(export #t)

(import
  :scheme/char
  :std/error :std/iter :std/misc/list :std/misc/ports :std/sort :std/srfi/1 :std/srfi/13 :std/sugar
  :clan/utils/base :clan/utils/list :clan/utils/number)

(def dictionary-path "/debian/usr/share/dict/american-english")

(def (dictionary-words)
  (read-file-lines dictionary-path))

(def (all-ascii-letters? string)
  (string-every char-alphabetic? string))

(def (delete-consecutive-duplicates list (duplicates? equal?))
  (with-list-builder (c)
    (match list
      ([] [])
      ([head . tail]
       (c head)
       (let loop ((previous head) (list tail))
         (match list
           ([] (void))
           ([head . tail]
            (if (duplicates? previous head)
              (loop previous tail)
              (begin
                (c head)
                (loop head tail))))))))))

(defonce (dictionary-simple-words)
  (delete-consecutive-duplicates
   (sort
    (map string-downcase (filter all-ascii-letters? (dictionary-words)))
    string<)))

(def ascii-a 97)
(def (letter<-index i) (integer->char (+ i ascii-a)))
(def (index<-letter l) (- (char->integer l) ascii-a))

(def (letter-counts)
  (let ((counts (make-vector 26 0)))
    (for-each
      (λ (word)
        (string-for-each
         (λ (c) (let ((i (index<-letter c)))
                  (vector-set! counts i (+ 1 (vector-ref counts i)))))
         word))
      (dictionary-simple-words))
    counts))

(def (letters-by-count)
  (let ((counts (letter-counts)))
    (sort
     (with-list-builder (c)
       (for (i (in-range 0 26))
         (c [(letter<-index i) (vector-ref counts i)])))
     (comparing-key test: > key: second))))

(def first-26-primes #(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101))

(def letter-primes
  (let ((vec (make-vector 26 #f))
        (i 0))
    (for-each
      (λ-match ([letter _]
                (vector-set! vec (index<-letter letter) (vector-ref first-26-primes i))
                (increment! i)))
      (letters-by-count))
    vec))

;;(def (prime<-letter letter) (vector-ref first-26-primes (index<-letter letter)))
(def (prime<-letter letter) (vector-ref letter-primes (index<-letter letter)))

(def (simple-word<-string x)
  (string-filter char-alphabetic? (string-downcase x)))

(def (number<-string x)
  (foldl * 1 (map prime<-letter (string->list (simple-word<-string x)))))

(def (max-word)
  (foldl (λ (x y) (if (> (cadr x) (cadr y)) x y))
         ["" 1]
         (map (λ (w) [w (number<-string w)]) (dictionary-simple-words))))

(def anagrams? (comparing-key key: number<-string))
