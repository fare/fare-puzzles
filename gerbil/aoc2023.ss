;; Solutions to https://AdventOfCode.com/2023 -*- Gerbil -*-

(import
  (for-syntax :std/iter :std/misc/number)
  :gerbil/gambit
  :std/assert
  :std/contract
  :std/debug/DBG
  :std/error
  :std/format
  :std/iter
  :std/misc/bytes
  :std/misc/func
  :std/misc/list
  :std/misc/number
  :std/misc/path
  :std/misc/ports
  :std/misc/repr
  :std/misc/string
  :std/parser/ll1
  :std/sort
  :std/source
  :std/srfi/1
  :std/srfi/13
  :std/srfi/43
  :std/sugar
  :std/test
  :std/text/basic-printers
  :std/text/char-set
  :clan/assert
  :clan/base
  :clan/matrix)

;;; General purpose utilities
(def (day-input-file n) (subpath (this-source-directory) (format "data/aoc2023-~d.input" n)))
(def (day-input-string n) (read-file-string (day-input-file n)))

(def u8vector-ref-set! u8vector-set!)
(def vector-ref-set! vector-set!)
(def (+/list l) (foldl + 0 l))

;;; DAY 1 https://adventofcode.com/2023/day/1
(def digit-names #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
;; First approach: first filter the string for digits, then extract first and last
;; NB: I first tried to do a rewrite with regexp or a loop,
;; but that yielded the wrong result because of overlaps
(def (filter-digits line)
  (string-filter char-ascii-digit line))
(def (string-first s)
  (and (positive? (string-length s)) (string-ref s 0)))
(def (string-last s)
  (and (positive? (string-length s)) (string-ref s (1- (string-length s)))))
(def (char-value c) (or (char-ascii-digit c) 0))
(def (filter-digits* s)
  (def l (string-length s))
  (with-output (o #f)
    (def (p c) (display c o))
    (let loop ((i 0))
      (when (< i l)
        (cond
         ((char-ascii-digit (string-ref s i))
          (p (string-ref s i))
          (loop (1+ i)))
         ((vector-index (lambda (name) (string-prefix? name s 0 (string-length name) i l)) digit-names) =>
          (lambda (d)
            (p (digit-char d))
            ;; overlaps allowed, so NOT (loop (+ i (string-length (vector-ref digit-names d))))
            (loop (1+ i))))
         (else
          (loop (1+ i))))))))
(def (calibration-value frobbed-line)
  (+ (* 10 (char-value (string-first frobbed-line)))
     (char-value (string-last frobbed-line))))
(def (day1.0 frob input)
  (+/list (map (compose calibration-value frob) (ll1/string ll1-lines input))))
(def (day1.1 input) (day1.0 filter-digits input))
(def (day1.2 input) (day1.0 filter-digits* input))

;; Better approach: instead of filter, just search a digit (or digit-name) from beginning and from end.
(def (first-digit string)
  (char-ascii-digit (string-ref string (string-index string char-ascii-digit))))
(def (last-digit string)
  (char-ascii-digit (string-ref string (string-index-right string char-ascii-digit))))
(def (first-digit* s)
  (def l (string-length s))
  (let loop ((i 0))
    (cond
     ((= i l) 0)
     ((char-ascii-digit (string-ref s i)))
     ((vector-index (lambda (name) (string-prefix? name s 0 (string-length name) i l)) digit-names))
     (else (loop (1+ i))))))
(def (last-digit* s)
  (let loop ((i (string-length s)))
    (cond
     ((zero? i) 0)
     ((char-ascii-digit (string-ref s (1- i))))
     ((vector-index (lambda (name) (string-suffix? name s 0 (string-length name) 0 i)) digit-names))
     (else (loop (1- i))))))
(def (calibration-value* first last line)
  (+ (* 10 (first line)) (last line)))
(def (day1.0* first last input)
  (+/list (map (cut calibration-value* first last <>) (ll1/string ll1-lines input))))
(def (day1.1* input) (day1.0* first-digit last-digit input))
(def (day1.2* input) (day1.0* first-digit* last-digit* input))
(defrule (check-day1 x1 x2)
  (begin
    (check (x1 "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet") => 142)
    (check (x2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen") => 281)))
(def (day1 (input (day-input-string 1)))
  (check-day1 day1.1 day1.2)
  (check-day1 day1.1* day1.2*)
  (check (day1.1 input) => (day1.1* input))
  (check (day1.2 input) => (day1.2* input))
  [(day1.1* input) (day1.2* input)])

;;; DAY 2 https://adventofcode.com/2023/day/2
;; Parser for Day 2 Games
(def (ll1-color color)
  (ll1-begin (ll1-string (as-string color)) (ll1-pure color)))
(def ll1-color-drawing
  (ll1-bind ll1-uint
            (lambda (n)
              (ll1* cons
                    (ll1-begin (ll1-string " ")
                               (ll1-peek "rgb")
                               (ll1-or (ll1-color 'red)
                                       (ll1-color 'green)
                                       (ll1-color 'blue)))
                    (ll1-result n)))))
(def ll1-end-of-drawing
  (ll1-peek [#!eof ";\n\r"]))
(def ll1-drawing ;; Parse each drawing to an alist
  (ll1-separated ll1-color-drawing (ll1-string ", ") ll1-end-of-drawing))
(def ll1-game
  (ll1* cons
   (ll1-begin (ll1-string "Game ") ll1-uint)
   (ll1-begin (ll1-string ": ")
              (ll1-separated ll1-drawing (ll1-string "; ") ll1-eolf?))))
(def ll1-games (ll1-repeated (ll1-begin0 ll1-game ll1-eolf) ll1-eof))
;; Day 2 Part 1
(def (possible-drawing? content drawing)
  (andmap (match <> ([color . n] (<= n (assgetq color content)))) drawing))
(def (possible-game? content game)
  (andmap (cut possible-drawing? content <>) (cdr game)))
(def (sum-possible-games content games)
  (+/list (map car (filter (cut possible-game? content <>) games))))
(def (day2.1 input)
  (sum-possible-games '((red . 12) (green . 13) (blue . 14)) (ll1/string ll1-games input)))
;; Day 2 Part 2
(def (game-minima game)
  (def h (hash))
  (def f (match <> ([c . n] (unless (<= n (hash-ref h c 0)) (hash-put! h c n)))))
  (for-each (lambda (drawing) (for-each f drawing)) (cdr game))
  h)
(def (game-power game)
  (foldl * 1 (hash-values (game-minima game))))
(def (day2.2 input)
  (+/list (map game-power (ll1/string ll1-games input))))
;; Wrapping up Day 2
(def day2-example "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
(def (day2 (input (day-input-string 2)))
  (check (day2.1 day2-example) => 8)
  (check (day2.2 day2-example) => 2286)
  (def r [(day2.1 input) (day2.2 input)])
  (check r => [2505 70265])
  r)

;;; DAY 3 https://adventofcode.com/2023/day/3
;; Parser for 2d string array
(def (parse-day3 string)
  (def rows (ll1/string ll1-lines string))
  (def n-rows (length rows))
  (check-argument-positive-integer n-rows)
  (def n-cols (string-length (first rows)))
  (check-argument-positive-integer n-cols)
  (check-argument (andmap (lambda (row) (equal? (string-length row) n-cols)) rows)
                  "equal length rows" string)
  (vector n-cols n-rows (string-concatenate rows)))
(def (i<-xy x y X Y) (+ x (* y X)))
(def (d3get v x y)
  (with ((vector X Y V) v)
    (and (exact-integer? x) (< -1 x X) (exact-integer? y) (< -1 y Y)
         (string-ref V (i<-xy x y X Y)))))
;; Day 3 Iterators -- in lieu of a data structure
(def (d3-for-each-number fun v)
  (with ((vector X Y V) v)
    (for ((y (in-range Y)))
      (let loop ((xstart 0))
        (when (< xstart X)
          (if (char-ascii-digit (d3get v xstart y))
            (let* ((xendy (string-index V (lambda (x) (not (char-ascii-digit x)))
                                        (1+ (i<-xy xstart y X Y)) (i<-xy X y X Y)))
                   (xend (if xendy (- xendy (i<-xy 0 y X Y)) X))
                   (n (string->number (substring V (i<-xy xstart y X Y) (i<-xy xend y X Y)))))
              (fun n xstart xend y)
              (loop (1+ xend)))
            (loop (1+ xstart))))))))
(def (part-symbol? x)
  (and (char? x) (not (or (eqv? x #\.) (char-ascii-digit x)))))
(def (d3-for-each-adjacent-symbol fun v xstart xend y)
  (with ((vector X Y V) v)
    (def (c gx gy)
      (let (g (d3get v gx gy))
        (when (part-symbol? g)
          (fun g gx gy))))
    (c (1- xstart) y)
    (c xend y)
    (for ((i (in-range (1- xstart) (1+ xend))))
      (c i (1- y))
      (c i (1+ y)))))
;; Day 3 Part 1
(def (has-adjacent-symbol? v xstart xend y)
  (let/cc return
    (d3-for-each-adjacent-symbol (lambda _ (return #t)) v xstart xend y)
    #f))
(def (day3.1 input)
  (def v (parse-day3 input))
  (def sum 0)
  (def (process-num n xstart xend y)
    (when (has-adjacent-symbol? v xstart xend y)
      (increment! sum n)))
  (d3-for-each-number process-num v)
  sum)
;; Day 3 Part 2
(def (day3.2 input)
  (def v (parse-day3 input))
  (def stars (hash))
  (def (process-num n xstart xend y)
    (def (process-symbol g gx gy)
      (when (eqv? g #\*)
        (hash-ensure-modify! stars [gx . gy] (lambda _ '()) (cut cons n <>))))
    (d3-for-each-adjacent-symbol process-symbol v xstart xend y))
  (d3-for-each-number process-num v)
  (+/list (map (match <> ([x y] (* x y)))
               (filter (cut length=n? <> 2)
                       (hash-values stars)))))
;; Wrapping up Day 3
(def day3-example "\
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")
(def (day3 (input (read-file-string (day-input-file 3))))
  (check (day3.1 day3-example) => 4361)
  (check (day3.2 day3-example) => 467835)
  [(day3.1 input) (day3.2 input)])

;;; DAY 4 https://adventofcode.com/2023/day/4
(def day4-example "\
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")
;; Day 4 Parse
(def ll1-d4-card
  (ll1-list (ll1-begin (ll1-string "Card ") ll1-skip-space* ll1-uint)
            (ll1-begin (ll1-string ":") ll1-skip-space*
                       (ll1-repeated (ll1-begin0 ll1-uint ll1-skip-space*) (ll1-string "|")))
            (ll1-repeated (ll1-begin ll1-skip-space* ll1-uint) ll1-eolf?)))
(def ll1-d4 (cut ll1-lines <> ll1-d4-card))
(def (d4-parse input) (ll1/string ll1-d4 input))
;; Day 4 Part 1
(def (card-matches card)
  (match card ([_ winning have] (length (lset-intersection = winning have)))))
(def (day4.1 cards)
  (def (d4-score n-matches)
    (if (zero? n-matches) 0 (arithmetic-shift 1 (1- n-matches))))
  (+/list (map (compose d4-score card-matches) cards)))
;; Day 4 Part 2
(def (day4.2 cards)
  (def len (length cards))
  (let loop ((sum 0) (len len) (cards cards) (counts (repeat 1 len)))
    (if (zero? len) sum
        (with ([card . card*] cards)
          (with ([count . count*] counts)
            (def matches (card-matches card))
            (defvalues (won more) (split-at count* (min len matches)))
            (loop (+ sum count) (1- len) card* (append (map (cut + <> count) won) more)))))))
;; Day 4 Wrap
(def (day4 (input (day-input-string 4)))
  (def example (d4-parse day4-example))
  (check (day4.1 example) => 13)
  (check (day4.2 example) => 30)
  (def cards (d4-parse input))
  [(day4.1 cards) (day4.2 cards)])
