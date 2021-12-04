;; Solutions to https://AdventOfCode.com/2021

(import
  :gerbil/gambit/bits
  :std/assert :std/format :std/iter :std/misc/list :std/misc/ports :std/misc/repr :std/misc/string
  :std/sort :std/srfi/1 :std/srfi/43 :std/sugar
  :clan/assert :clan/base :clan/basic-parsers :clan/basic-printers
  :clan/debug :clan/number
  :clan/source)

;;; General purpose utilities

(def (day-input-file n) (path-expand (format "data/aoc2021-~d.input" n) (this-source-directory)))
(def bytes-ref-set! bytes-set!)

;;; DAY 1 https://adventofcode.com/2021/day/1
(def (count-increases l)
  (let loop ((i 0) (prev #f) (l l))
    (match l
      ([curr more ...] (loop (if (and prev (< prev curr)) (1+ i) i) curr more))
      ([] i))))

(def (3-sums l)
  (match l
    ([a b ll ...]
     (let loop ((a a) (b b) (l ll) (r []))
       (match l
         ([c m ...] (loop b c m (cons (+ a b c) r)))
         ([] (reverse r)))))))

(def (day1 (input (parse-file-lines expect-natural (day-input-file 1))))
  (def (answer1) (count-increases input))
  (def (answer2) (count-increases (3-sums input)))
  [(answer1) (answer2)])
(assert-equal! (day1 '(199 200 208 210 200 207 240 269 260 263))
               '(7 5))

;;; DAY 2 https://adventofcode.com/2021/day/2
(def (validate-direction dir)
  (assert! (member dir '(forward up down)))
  dir)
(def (direction<-string s)
  (validate-direction (string->symbol s)))
(def (expect-command port)
  (def direction (direction<-string ((expect-one-or-more-of char-alphabetic?) port)))
  (expect-and-skip-any-whitespace port)
  (def amount (expect-signed-integer port))
  (list direction amount))
(def (update-position-1 command position)
  (match position
    ([horizontal depth]
     (match command
       (['forward n] [(+ horizontal n) depth])
       (['up n] [horizontal (- depth n)])
       (['down n] [horizontal (+ depth n)])))))
(def (update-position command position)
  (match position
    ([horizontal depth aim]
     (match command
       (['forward n] [(+ horizontal n) (+ depth (* n aim)) aim])
       (['up n] [horizontal depth (- aim n)])
       (['down n] [horizontal depth (+ aim n)])))))

(def (day2 (input (parse-file-lines expect-command (day-input-file 2))))
  (def (answer1)
    (match (foldl update-position-1 [0 0] input)
      ([horizontal depth]
       ;;(DBG 'final-position-1 horizontal depth)
       (* horizontal depth))))
  (def (answer2)
    (match (foldl update-position [0 0 0] input)
      ([horizontal depth aim]
       ;;(DBG 'final-position-2 horizontal depth aim)
       (* horizontal depth))))
  [(answer1) (answer2)])
(assert-equal! (day2 '((forward 5) (down 5) (forward 8) (up 3) (down 8) (forward 2)))
               '(150 900))

;;; DAY 3 https://adventofcode.com/2021/day/3
(def vector-ref-set! vector-set!)

(def (day3 (input (read-file-lines (day-input-file 3))))
  (assert! (pair? input))
  (def bitlen (string-length (car input)))
  (assert! (every (lambda (s) (= (string-length s) bitlen)) input))
  (def (parse-bits s) (parse-string s (cut expect-natural <> 2)))
  (def input-bits (map parse-bits input))
  (def (most-common-bit lst pos tie-error: (tie-error #t))
    (def len (length lst))
    (assert! (plus? len))
    (def shift (- pos bitlen -1))
    (def count (foldl (lambda (bits acc)
                        (+ acc (bitwise-and 1 (arithmetic-shift bits shift))))
                      0 lst))
    (def diff (- (+ count count) len))
    ;;(DBG "mcb" bitlen pos tie-error len shift count diff)
    (cond
     ((plus? diff) 1)
     ((minus? diff) 0)
     (tie-error (error "As many 0's and 1's at position of list" pos lst))
     (else 1)))
  (def gamma (let loop ((g 0) (pos 0) (x (arithmetic-shift 1 (1- bitlen))))
               (if (= pos bitlen) g
                   (loop (if (plus? (most-common-bit input-bits pos)) (+ g x) g)
                         (1+ pos) (arithmetic-shift x -1)))))
  (def epsilon (- (arithmetic-shift 1 bitlen) 1 gamma))
  ;;(DBG "d3.1" bitlen (format "~b" gamma) (format "~b" epsilon) gamma epsilon (* gamma epsilon))
  (def answer1 (* gamma epsilon)) ;; 3148794
  (def oxygen-generator-rating
    (let loop ((lst input-bits) (ogr 0) (pos 0) (x (arithmetic-shift 1 (1- bitlen))))
      (if (zero? x) ogr
          (let* ((mcb (most-common-bit lst pos tie-error: #f))
                 (xm (if (plus? mcb) x 0))
                 (subl (filter (lambda (bits) (= xm (bitwise-and bits x))) lst)))
            (loop subl (+ ogr xm) (1+ pos) (arithmetic-shift x -1))))))
  (def co2-scrubber-rating
    (let loop ((lst input-bits) (csr 0) (pos 0) (x (arithmetic-shift 1 (1- bitlen))))
      ;;(DBG csr1: csr pos x (map (cut format "~b" <>) lst))
      (cond
       ((or (null? lst) (zero? x)) csr)
       ((null? (cdr lst)) (car lst))
       (else
        (let* ((mcb (most-common-bit lst pos tie-error: #f))
               (xl (if (plus? mcb) 0 x))
               (subl (filter (lambda (bits) (= xl (bitwise-and bits x))) lst)))
          ;;(DBG csr2: mcb xl (map (cut format "~b" <>) subl))
          (loop subl (+ csr xl) (1+ pos) (arithmetic-shift x -1)))))))
  ;;(DBG "d3.2" oxygen-generator-rating co2-scrubber-rating)
  (def answer2 (* oxygen-generator-rating co2-scrubber-rating))
  [answer1 answer2])
(assert-equal! (day3 '("00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"))
               '(198 230))

;; Day 4
(def (validate-5-list l) (assert! (length=n? l 5)) l)
(def expect-comma-separated-naturals-line
  (expect-separated expect-natural (expect-char #\,) expect-eol))
(def expect-card-line
  (expect-begin0
   (expect-n-repeats 5 (expect-begin expect-and-skip-any-whitespace expect-natural))
   expect-eol))
(def expect-card (expect-n-repeats 5 expect-card-line))
(def expect-cards (expect-repeated (expect-begin expect-eol expect-card) expect-eof))
;; line-pattern-index: 5 vertical, 5 horizontal (NO 2 diagonals)
;; bingo-card-score: vector [0..5] <- line-pattern-index | [0..10] <- '0, count found per line pattern, line patterns filled
;; card-pattern: table (list line-pattern-index) <- natural
;; master-card-pattern: from position of number in flattened list of bingo card to (list line-pattern-index)
(def (empty-bingo-card-score . _) (make-bytes 13 0))
(def master-card-pattern
  (list->vector
   (with-list-builder (c)
     (for (y (in-range 5))
       (for (x (in-range 5))
         (c (list->vector
             (with-list-builder (l)
               (l (1+ x)) (l (+ 6 y))
               ;;(when (= x y) (l 11)) (when (= x (- 4 y)) (l 12))
               ))))))))
(def card-positions<-line-pattern-index
  (list->vector
   (with-list-builder (c)
     (c #()) ;; 0 is not a line-pattern index
     (for ((x (in-range 5))) (c (list->vector (for/collect ((y (in-range 5))) (+ x (* 5 y)))))) ; vertical
     (for ((y (in-range 5))) (c (list->vector (for/collect ((x (in-range 5))) (+ x (* 5 y)))))) ; horizontal
     ;;     (c #(0 6 12 18 24)) ;; x=y diagonal
     ;;     (c #(4 8 12 16 20)) ;; x=4-y diagonal
     )))
(def (card-pattern<-card _ card) ;; assumes cards are 5x5 and don't repeat numbers
  (def table (hash))
  (for ((val (concatenate card))
        (i (in-naturals)))
    (hash-put! table val (vector-ref master-card-pattern i)))
  table)
(def (update-bingo-card-score! draw card-pattern bingo-card-score)
  (for ((line-pattern-index (hash-ref card-pattern draw #())))
    (def line-pattern-score (1+ (bytes-ref bingo-card-score line-pattern-index)))
    (bytes-set! bingo-card-score line-pattern-index line-pattern-score)
    (when (= line-pattern-score 5)
      (increment! (bytes-ref bingo-card-score 0)))))
(def (bingo-winner? bingo-card-score)
  (plus? (bytes-ref bingo-card-score 0)))

(def (day4 (input (read-file-string (day-input-file 4))))
  (def (expect-day4-input port)
    (def order (expect-comma-separated-naturals-line port))
    (def cards (expect-cards port))
    (values order cards))
  (defvalues (order cards) (parse-string input expect-day4-input))
  ;;(DBG "d4" order cards)
  (def card% (list->vector cards))
  (def card-pattern% (vector-map card-pattern<-card card%))
  (def (bingo-score draw order n-draws card)
    (* draw (reduce + 0 (lset-difference = (concatenate card) (take order n-draws)))))
  (def (answer1)
    (def bingo-card-score% (vector-map empty-bingo-card-score card%))
    (let/cc return
      (for ((draw order)
            (n (in-naturals 1)))
        (for ((card-pattern card-pattern%)
              (card card%)
              (bingo-card-score bingo-card-score%))
          ;;(DBG ubcs>: draw card bingo-card-score)
          (update-bingo-card-score! draw card-pattern bingo-card-score)
          ;;(DBG ubcs<: bingo-card-score)
          (when (bingo-winner? bingo-card-score)
            (return (bingo-score draw order n card)))))))
  (def (answer2)
    (def bingo-card-score% (vector-map empty-bingo-card-score card%))
    (def losers (vector-length card%))
    (let/cc return
      (for ((draw order)
            (n (in-naturals 1)))
        (for ((card-pattern card-pattern%)
              (card card%)
              (bingo-card-score bingo-card-score%))
          (unless (bingo-winner? bingo-card-score)
            (update-bingo-card-score! draw card-pattern bingo-card-score)
            (when (bingo-winner? bingo-card-score)
              (decrement! losers)
              (when (= losers 0)
                (return (bingo-score draw order n card)))))))))
  [(answer1) (answer2)])

(assert-equal! (day4 "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7") '(4512 1924))
