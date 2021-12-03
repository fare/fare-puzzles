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
  (def (answer2) (void)) ;;(count-increases (3-sums input)))
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
