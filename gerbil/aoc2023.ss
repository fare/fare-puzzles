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
  :std/misc/evector
  :std/misc/func
  :std/misc/hash
  :std/misc/list
  :std/misc/number
  :std/misc/path
  :std/misc/ports
  :std/misc/repr
  :std/misc/string
  :std/misc/vector
  :std/parser/ll1
  :std/sort
  :std/source
  :std/srfi/1 ; lists
  :std/srfi/13 ; strings
  :std/srfi/133 ; vectors
  :std/srfi/141 ; integer division
  :std/sugar
  :std/test
  :std/text/basic-printers
  :std/text/char-set
  :clan/memo
  :clan/order)

;;; General purpose utilities
(def (day-input-file n) (subpath (this-source-directory) (format "data/aoc2023-~d.input" n)))
(def (day-input-string n) (read-file-string (day-input-file n)))

(def u8vector-ref-set! u8vector-set!)
(def vector-ref-set! vector-set!)
(def (+/list l) (foldl + 0 l))

;;; DAY 1 https://adventofcode.com/2023/day/1 -- simple string search
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

;;; DAY 2 https://adventofcode.com/2023/day/2 -- parsing and iterating
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

;;; DAY 3 https://adventofcode.com/2023/day/3 -- iteration over 2d array
;; Parser for 2d string array
(def (parse-2d-string string) ;; : String -> (Vector-Tuple UInt UInt String)
  (def rows (ll1/string ll1-lines string))
  (def n-rows (length rows))
  (check-argument-positive-integer n-rows)
  (def n-cols (string-length (first rows)))
  (check-argument-positive-integer n-cols)
  (check-argument (andmap (lambda (row) (equal? (string-length row) n-cols)) rows)
                  "equal length rows" string)
  (vector n-cols n-rows (string-concatenate rows)))
(def (i<-xy x y X Y) (+ x (* y X)))
(def (xy<-i i X Y) (defvalues (y x) (floor/ i X)) (values x y))
(def (xyget v x y)
  (with ((vector X Y V) v)
    (and (exact-integer? x) (< -1 x X) (exact-integer? y) (< -1 y Y)
         (string-ref V (i<-xy x y X Y)))))
;; Day 3 Iterators -- in lieu of a data structure
(def (d3-for-each-number fun v)
  (with ((vector X Y V) v)
    (for ((y (in-range Y)))
      (let loop ((xstart 0))
        (when (< xstart X)
          (if (char-ascii-digit (xyget v xstart y))
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
      (let (g (xyget v gx gy))
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
  (def v (parse-2d-string input))
  (def sum 0)
  (def (process-num n xstart xend y)
    (when (has-adjacent-symbol? v xstart xend y)
      (increment! sum n)))
  (d3-for-each-number process-num v)
  sum)
;; Day 3 Part 2
(def (day3.2 input)
  (def v (parse-2d-string input))
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

;;; DAY 4 https://adventofcode.com/2023/day/4 -- counting and recursion
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

;;; DAY 5 https://adventofcode.com/2023/day/5 -- merging intervals
;; Day 5 Parsing
(def (ll1-string-val string val) (ll1-begin (ll1-string string) (ll1-pure val)))
(def ll1-uints-line (ll1-repeated (ll1-begin ll1-skip-space* ll1-uint) ll1-eolf))
(def ll1-name (ll1* make-symbol (ll1-char+ char-ascii-alphabetic?)))
(def ll1-d5map
  (ll1-begin
   (ll1* cons*
         (ll1-begin0 ll1-name (ll1-string "-to-"))
         (ll1-begin0 ll1-name (ll1-string " map:\n"))
         (ll1-repeated ll1-uints-line ll1-eolf))))
(def ll1-seeds (ll1* cons (ll1-string-val "seeds:" 'seeds) (ll1-begin0 ll1-uints-line ll1-eol)))
(def ll1-day5 (ll1* cons ll1-seeds (ll1-repeated ll1-d5map ll1-eof)))
(def (day5-parse input) (ll1/string ll1-day5 input))
;; Day 5 Part 1
(def (d5-map map val)
  (let/cc return
    (for ([dst src len] (cddr map))
      (when (<= src val (+ src len))
        (return (+ dst (- val src)))))
    val))
(def (d5-all-maps val maps) (foldl d5-map val maps))
(def (day5.1 almanac)
  (with ([[_ . seeds] . maps] almanac)
    (apply min (map (cut d5-all-maps <> maps) seeds))))
;; Day 5 Part 2
;; We use a list of intervals, because there aren't many of them.
;; If there were a lot, we'd use balanced trees plus zippers.
;; Representation: List of [src . off] which maps the interval upto the next [src . off]
;; or to +inf.0 for the last entry, with off being the (- dst src) offset.
;; NB: instead of "multiply by a scalar" we have "add a scalar" (with +inf.0 as absorbing element).
(def d5m-0 []) ;; same as [[-inf.0 . +inf.0]] ;; compositional 0 (absorbing element)
(def d5m-1 [[-inf.0 . 0]]) ;; same as [[-inf.0 . 0]] ;; compositional 1 (identity element)
(def (d5m-offset offset) (if (= offset +inf.0) [] [[-inf.0 . offset]])) ;; constant offset
(def (d5m-paste mhead cut mtail) ;; paste mhead until cut (excluded) then mtail
  (def (head off m)
    (match m
      ([] (tail off +inf.0 mtail))
      ([[src0 . off0] . m0]
       (if (>= src0 cut) (tail off +inf.0 mtail)
           [[src0 . off0] :: (head off0 m0)]))))
  (def (tail offh offt m)
    (match m
      ([] (paste offh offt []))
      ([[src0 . off0] . m0]
       (cond
        ((< src0 cut) (tail offh off0 m0))
        ((= src0 cut) (paste offh off0 m0))
        (else #|(> src0 cut)|# (paste offh offt m))))))
  (def (paste offh offt m)
    (if (= offh offt) m [[cut . offt] . m]))
  (cond
   ((= cut +inf.0) mhead)
   ((= cut -inf.0) mtail)
   (else (head +inf.0 mhead))))
(def (d5m-set-range off src end m) ;; override one range
  (d5m-paste m src (d5m-paste (d5m-offset off) end m)))
(def (d5m-shift s m) ;; x -> o + m( x + s )
  (if (= s +inf.0) []
      (map (match <> ([src . off] [(- src s) :: (+ off o s)])) m)))
(def d5m<-seeds
  (match <> ([] d5m-0)
         ([src len . r] (d5m-set-range 0 src (+ src len) (d5m<-seeds r)))))
(def (d5m<-map map)
  (let loop ((ranges (cddr map)))
    (match ranges ([] d5m-1)
           ([[dst src len] . r] (d5m-set-range (- dst src) src (+ src len) (loop r))))))
(def (d5m-normalize m)
  (match m ([[_ . +inf.0] . mm] mm) (else m)))
(def (d5m* m1 m2) ;; (compose m1 m2)
  (match m2
    ([] [])
    ([[src . off] . m2r]
     (let (m1l (d5m-shift off (d5m-paste [] (+ off src) m1)))
       (match m2r
         ([] m1l)
         ([[end . _] . _] (d5m-paste m1l end (d5m* m1 (d5m-normalize m2r)))))))))
(def (d5-all-maps/intervals intervals maps) (foldl d5m* intervals maps))
(def (d5m-min d5m)
  (xmin/list (map (match <> ([off . src] (+ off src))) d5m)))
(def (day5.2 almanac)
  (with ([[_ . seeds] . maps] almanac)
    (d5m-min (foldl d5m* (d5m<-seeds seeds) (map d5m<-map maps)))))
;; Day 5 Wrap up
(def day5-example "\
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
")
(def (day5 (input (day-input-string 5)))
  (def example (day5-parse day5-example))
  (check (day5.1 example) => 35)
  (check (day5.2 example) => 46)
  (def almanac (day5-parse input))
  [(day5.1 almanac) (day5.2 almanac)])

;;; DAY 6 https://adventofcode.com/2023/day/6 -- simple numeric optimization
;; Day 6 Parsing
(def ll1-day6 (ll1-list (ll1-begin (ll1-string "Time:") ll1-uints-line)
                        (ll1-begin (ll1-string "Distance:") ll1-uints-line)))
(def (day6-parse input) (ll1/string ll1-day6 input))
;; Day 6 Part 1
(def (d6-distance time charge) (* charge (- time charge)))
;; find the length of the x segment such that x * (t - x) > d
;; extremities are at: -x2 + t*x -d > 0 ;; ( -t ± √ t^2-4d )/-2 = (t ± √ t^2-4d )/2
(def (d6-ways-to-win time distance)
  (def discr (- (* time time) (* 4 distance)))
  (if (negative? discr) 0
      (let* ((sd (integer-sqrt discr))
             (down (ceiling (/ (- time sd) 2)))
             (up (floor (/ (+ time sd) 2)))
             (exact? (= (d6-distance time down) distance))
             (adj (if exact? 1 0))
             (down1 (+ down adj))
             (up1 (- up adj)))
        (if (>= up1 down1) (+ 1 (- up1 down1)) 0))))
(def (day6.1 races) (foldl * 1 (apply map d6-ways-to-win (day6-parse races))))
(def (day6.2 race) (day6.1 (string-delete #\space race)))
;; Day 6 Wrap up
(def day6-example "\
Time:      7  15   30
Distance:  9  40  200")
(def (day6 (input (day-input-string 6)))
  (check (day6.1 day6-example) => 288)
  (check (day6.2 day6-example) => 71503)
  #;(check (day6.2 example) => 46)
  [(day6.1 input) (day6.2 input)])

;;; DAY 7 https://adventofcode.com/2023/day/7 -- counting and sorting
;; Day 7 Parsing
(def cards7.1 (string-reverse "AKQJT98765432"))
(def cards7.2 (string-reverse "AKQT98765432J"))
(def ll1-day7 (cut ll1-lines <> (ll1-list (ll1-n-chars 5 cards7.1)
                                          (ll1-begin (ll1-string " ") ll1-uint))))
(def (day7-parse input) (ll1/string ll1-day7 input))
;; Day 7 Part 1
(def (effective-card-counts n card-counts)
  (let (l (vector->list card-counts))
    (case n ((1) (sort l >))
          ((2) (with ([j . k] l)
                 (with ([h . t] (sort k >))
                   [(+ j h) . t]))))))
(def (hand->list n h)
  (let* ((cards (case n ((1) cards7.1) ((2) cards7.2)))
         (card-nums (map (cut string-index cards <>) (string->list h)))
         (card-counts (make-vector 13 0))
         (_ (for-each (lambda (n) (increment! (vector-ref card-counts n))) card-nums))
         (counts (take (effective-card-counts n card-counts) 2))) ;; two largest counts are enough
    (append counts card-nums)))
(def ((compare-hand n) h1 h2) (lexicographic<? < (hand->list n h1) (hand->list n h2)))
(def ((compare-bids n) bid1 bid2) ((compare-hand n) (first bid1) (first bid2)))
(def (day7.n n bids)
  (+/list (map * (map second (sort bids (compare-bids n))) (iota (length bids) 1))))
(def (day7.1 bids) (day7.n 1 bids))
(def (day7.2 bids) (day7.n 2 bids))
;; Day 7 Wrap up
(def day7-example "\
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")
(def (day7 (input (day-input-string 7)))
  (def example (day7-parse day7-example))
  (def bids (day7-parse input))
  (check (day7.1 example) => 6440)
  (check (day7.2 example) => 5905)
  [(day7.1 bids) (day7.2 bids)])

;;; DAY 8 https://adventofcode.com/2023/day/8 -- finite state machines, deterministic and non-
;; Day 8 Parsing
(def ll1-d8node (ll1-n-chars 3 char-ascii-alphanumeric?))
(def ll1-d8nodespec
  (ll1-list (ll1-begin0 ll1-d8node (ll1-string " = ("))
            (ll1-begin0 ll1-d8node (ll1-string ", "))
            (ll1-begin0 ll1-d8node (ll1-string ")") ll1-eolf)))
(def ll1-day8 (ll1-list (ll1-begin0 (ll1-char* "LR") ll1-eol ll1-eol)
                        (ll1-repeated ll1-d8nodespec ll1-eof)))
(def (day8-parse input) (ll1/string ll1-day8 input))
;; Day 8 Part 1
(def (day8.1 machine)
  (with ([path nodes] machine)
    (def h (list->hash-table nodes))
    (def l (string-length path))
    (let loop ((i 0) (node "AAA"))
      (if (equal? node "ZZZ") i
          (loop (1+ i) ((case (string-ref path (modulo i l)) ((#\L) first) ((#\R) second))
                        (hash-get h node)))))))
;; Day 8 Part 2
;; From each start, a cycle of length N is found after M steps or the full path of length P,
;; The total cycle is thus actually of length P*N, and we find exit nodes at regular intervals
;; within, plus possibly during the pre-cycle phase.
;; Once all the cycles and the M's are found,
;; we can solve co-exit on all of them. If S0 has M0P N0P, S1 has M1P N1P, then,
;; N=LCM(N0P,N1P)=A0*N0P=A1*N1P, M=max(M0P,M1P), new state table
;; NB: Can simultaneously do all of a cycle at once for result in O(n (log n)^k)
;; In the end: look whether there's a solution of length < MP, otherwise, pick the first in NP
;; Also, sparse(?) lists of exits.
;; And end-form tells you all the ends from a given start
(defstruct end-form (precycle-len precycle-ends cycle-len cycle-ends) transparent: #t)
(def end-form-id (end-form 0 [] 1 [0])) ;; everything is an end!
(def (end-form-min ef) ;; the first solution in a end-form
  (with ((end-form pl pe _ e) ef)
    (if (pair? pe) (first pe)
      (if (pair? e) (+ pl (first e))
          +inf.0))))
(def (repeat-ends n l e) ;; repeat the ends e for n cycles of length l
  (cond ((or (zero? n) (null? e)) [])
        ((= n 1) e)
        (else (append-map (lambda (i) (def p (* i l)) (map (cut + p <>) e)) (iota n)))))
(def (shift-ends s e) ;; shift ends by s
  (if (zero? s) e (map (cut + <> s) e)))
(def (rotate-ends s l e) ;; rotate the ends e in a cycle of length l by s
  (let* ((s (modulo s l))
         (l-s (- l s))
         ((values e- e+) (span (cut < <> s) e)))
    (append (shift-ends (- s) e+) (shift-ends l-s e-))))
(def (shift-end-form s ef) ;; shift an end-form to extend its pre-cycle length
  (check-argument-uint s)
  (with ((end-form pl pe l e) ef)
    (let* ((ee (rotate-ends s l e))
           (n (ceiling-quotient s l))
           (er (repeat-ends n l ee)))
      (end-form (+ pl s) (append pe (shift-ends pl (take-while (cut < <> s) er))) l ee))))
(def (intersect-sorted-lists e0 e1)
  (def (a e0 e1) (match e0 ([x0 . r0] (b x0 r0 e1)) ([] [])))
  (def (b x0 r0 e1) (match e1 ([x1 . r1] (c x0 r0 x1 r1)) ([] [])))
  (def (c x0 r0 x1 r1)
    (cond ((= x0 x1) (cons x0 (a r0 r1)))
          ((< x0 x1) (b x1 r1 r0))
          (else #|(> x0 x1)|# (b x0 r0 r1))))
  (a e0 e1))
(def (merge-end-forms ef0 ef1)
  (with ((end-form pl0 _ l0 _) ef0)
    (with ((end-form pl1 _ l1 _) ef1)
      (defvalues (pl ef0+ ef1+) ;; align the two end-forms to the same prefix length
        (if (< pl0 pl1)
          (values pl1 (shift-end-form (- pl1 pl0) ef0) ef1)
          (values pl0 ef0 (shift-end-form (- pl0 pl1) ef1))))
      (def pe (intersect-sorted-lists (end-form-precycle-ends ef0+) (end-form-precycle-ends ef1+)))
      (def l (lcm l0 l1))
      (defvalues (j0 j1 g) (bezout l0 l1)) ;; j0*l0 + j1*l1 = g
      (def j1l1 (* j1 l1))
      ;; x in e == Ex x0 in ef0+, Ex i0 in [0, a0),
      ;;             Ex x1 in ef1+, Ex i1 in [0, a1),   x = x0+i0*l0 = x1+i1*l1  [l]
      ;; x0-x1 = i0*l0 - i0*l1 ;; x0-x1 = q*g+r = q*j0*l0+q*j1*l1+r. If r!=0, bad.
      (def e (with-list-builder (c)
               (for ((x0 (end-form-cycle-ends ef0+)))
                 (for ((x1 (end-form-cycle-ends ef1+)))
                   (let (((values q r) (floor/ (- x0 x1) g)))
                     (when (zero? r)
                       (c (modulo (+ x1 (* j1l1 q)) l))))))))
      (end-form pl pe l (sort e <)))))
(defstruct node-info
  (name ;; TLA
   start? end? ;; is in a start? an end?
   L R succ ;; next node through L, through R, through the whole path.
   ends ;; list of ends within path
   precycle cycle cycle-pos) ;; iterations before cycle reached, head of cycle, position in cycle
  transparent: #t final: #t)
(def (info node)
  (with ([n L R] node)
    (node-info n (eqv? (string-ref n 2) #\A) (eqv? (string-ref n 2) #\Z)
               L R #f #f #f #f #f)))
(def (day8.2 machine)
  (def path (first machine))
  (def nodes (second machine))
  (defvalues (i->ni* Nstarts)
    (let-values (((starts non-starts) (partition node-info-start? (map info nodes))))
      (values (list->vector (append starts non-starts)) (length starts))))
  (defrule (i->ni i) (vector-ref i->ni* i))
  (def n->i* (invert-hash<-vector i->ni* key: node-info-name))
  (defrule (n->i n) (hash-get n->i* n))
  (def P (string-length path))
  (def N (vector-length i->ni*))
  (vector-for-each
   (lambda (ni)
     (set! (node-info-L ni) (n->i (node-info-L ni)))
     (set! (node-info-R ni) (n->i (node-info-R ni)))) i->ni*)
  (def (step i p)
    ((case (string-ref path p)
       ((#\L) node-info-L) ((#\R) node-info-R))
     (i->ni i)))
  (def (compute-succ/ends i)
    (def ni (i->ni i))
    (def e (list->evector '()))
    (let loop ((p 0) (j i))
      (cond ((= p P)
             (set! (node-info-ends ni) (evector->list e))
             (set! (node-info-succ ni) j)
             j)
            (else
             (when (node-info-end? (i->ni j)) (evector-push! e p)) ;; revert later?
             (loop (1+ p) (step j p))))))
  (def cycles (list->evector '()))
  (defrule (Ncycles) (evector-fill-pointer cycles))
  ;; Find all the cycles from the start points
  (for ((i (in-range Nstarts)))
    (let* ((n (Ncycles))
           (cycle (list->evector '())))
      (let loop ((i i))
        (let* ((ni (i->ni i))
               (c (node-info-cycle ni)))
          (cond
           ((eqv? n c) ;; new cycle completed!
            (let* ((cv (evector->vector cycle))
                   (cl (vector-length cv))
                   (head (vector-index (cut eqv? <> i) cv))
                   (elements (subvector cv head cl)))
              (for (h (in-range head))
                (let (nh (i->ni (vector-ref cv h)))
                  (set! (node-info-precycle nh) (- head h))
                  (set! (node-info-cycle-pos nh) 0)))
              (for (e (in-range head cl))
                (let (ne (i->ni (vector-ref cv e)))
                  (set! (node-info-precycle ne) 0)
                  (set! (node-info-cycle-pos ne) (- e head))))
              (evector-push! cycles (- cl head))))
           (c ;; prefix to old cycle found
            (let* ((pv (evector->vector cycle))
                   (pl (vector-length pv))
                   (cp (node-info-cycle-pos ni)))
              (for (p (in-range pl))
                (let (np (i->ni (vector-ref pv p)))
                  (set! (node-info-cycle np) c)
                  (set! (node-info-precycle np) (- pl p))
                  (set! (node-info-cycle-pos np) cp)))))
           (else ;; into the unknown
            (set! (node-info-cycle ni) n)
            (evector-push! cycle i)
            (loop (compute-succ/ends i))))))))
  (def (find-ends i l)
    (def e (list->evector '()))
    (let loop ((j i) (s 0) (l l))
      (if (zero? l)
        (values (evector->list e) j)
        (let ((nj (i->ni j)))
          (for-each (cut evector-push! e <>) (shift-ends s (node-info-ends nj)))
          (loop (node-info-succ nj) (+ s P) (1- l))))))
  (def (compute-end-form i)
    (def ni (i->ni i))
    (def pl (* P (node-info-precycle ni)))
    (defvalues (pe h) (find-ends i (node-info-precycle ni)))
    (def cl (evector-ref cycles (node-info-cycle ni)))
    (def l (* P cl))
    (defvalues (e _) (find-ends h cl))
    (end-form pl pe l e))
  (end-form-min (foldl merge-end-forms end-form-id (map compute-end-form (iota Nstarts)))))
;; Day 8 Wrap up
(def day8-example1 "\
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")
(def day8-example2 "\
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")
(def (day8 (input (day-input-string 8)))
  (def example1 (day8-parse day8-example1))
  (def example2 (day8-parse day8-example2))
  (check (day8.1 example1) => 6)
  (check (day8.2 example2) => 6)
  (def machine (day8-parse input))
  [(day8.1 machine) (day8.2 machine)])

;;; DAY 9 https://adventofcode.com/2023/day/9 -- Polynomial interpolation
;; https://en.wikipedia.org/wiki/Lagrange_polynomial
;; 0, a0, a1+k*a0, a2+k*a1+k(k+1)/2*a0, etc....
;; Sequences defined by simple integrations like that are polynomials.
;; Therefore, instead of going through the hard way of iterating, we can "just" interpolate,
;; and get a formula that works not just for the next or previous integer k,
;; but for all numbers (including complex, or more complex).
;; Day 9 Parsing
(def ll1-sints-line (ll1-repeated (ll1-begin ll1-skip-space* ll1-sint) ll1-eolf))
(def ll1-day9 (ll1-repeated ll1-sints-line ll1-eof))
(def (day9-parse input) (ll1/string ll1-day9 input))
;; Day 9 Part 1
(def fact (memoize-recursive-sequence (lambda (n) (* n (fact (1- n)))) cache: (list->evector '(1))))
;; Lagrange polynomial barycentric weight for x^j for interpolation points being the first k integers (starting at 0)
(def (weight j k) (let (l (- k j 1)) (/ (* (fact j) (fact l) (if (odd? l) -1 1)))))
(define-memo-function (weights k) (map (cut weight <> k) (iota k)))
(def (interpolate seq (x (length seq)))
  (def n (length seq))
  (let loop ((N 0) (D 0) (j 0) (ys seq) (ws (weights n)))
    (if (= j n) (/ N D)
        (with ([y . yr] ys)
          (with ([w . wr] ws)
            (let (z (/ w (- x j)))
              (loop (+ N (* y z)) (+ D z) (1+ j) yr wr)))))))
(def (day9.1 sequences)
  (+/list (map interpolate sequences)))
(def (day9.2 sequences)
  (+/list (map (cut interpolate <> -1) sequences)))
;; Day 9 Wrap up
(def day9-example "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")
(def (day9 (input (day-input-string 9)))
  (def example (day9-parse day9-example))
  (check (day9.1 example) => 114)
  (check (day9.2 example) => 2)
  (def sequences (day9-parse input))
  [(day9.1 sequences) (day9.2 sequences)])

;;; DAY 10 https://adventofcode.com/2023/day/10 -- Green's Theorem
(def xy+ (case-lambda
           (() '(0 . 0))
           ((p) p)
           ((p0 p1) (with ([x0 . y0] p0) (with ([x1 . y1] p1) (cons (+ x0 x1) (+ y0 y1)))))
           ((p0 . pr) (foldl xy+ p0 pr))))
(def xy- (case-lambda
           ((p) (with ([x . y] p) (cons (- x) (- y))))
           ((p0 p1) (with ([x0 . y0] p0) (with ([x1 . y1] p1) (cons (- x0 x1) (- y0 y1)))))
           ((p0 . pr) (xy- p0 (foldl xy+ '(0 . 0) pr)))))
(def wind-names #(North South East West)) ;; 0 1 2 3
(def wind-xy #((0 . -1) (0 . 1) (1 . 0) (-1 . 0)))
(def (-wind wind) (bitwise-xor wind 1))
(def (pos+wind pos wind) (xy+ pos (vector-ref wind-xy wind)))
(def pipe-chars "|-LJ7F.S")
(def pipe-connections #((0 1) (2 3) (0 2) (0 3) (1 3) (1 2) () (0 1 2 3)))
(def (pget v p) (with ([x . y] p) (xyget v x y)))
(def (pipe-get v p) (alet (c (pget v p)) (string-index pipe-chars c)))
(def (has-connection? v pos wind)
  (alet (pipe (pipe-get v pos))
    (member wind (vector-ref pipe-connections pipe))))
(def (find-wind v p winds)
  (find (lambda (w) (has-connection? v (pos+wind p w) (-wind w))) winds))
(def (day10* v f)
  (with ((vector X Y s) v)
    (defvalues (x y) (xy<-i (string-index s #\S) X Y))
    (def pos (cons x y))
    (def wind (find-wind v pos (iota 4)))
    (let loop ((pos pos) (wind wind) (sum 0))
      (def s (+ sum (f pos wind)))
      (def from (-wind wind))
      (def p (pos+wind pos wind))
      (def pipe (pipe-get v p))
      (if (equal? pipe 7) ;; #\S
        s
        (let (w (find-wind v p (delete from (vector-ref pipe-connections pipe))))
          (assert! (has-connection? v p from))
          (assert! w)
          (loop p w s))))))
(def (day10.1 pipes)
  (half (day10* pipes (lambda (_ _) 1))))
(def (green-darea pos wind) ;; applying Green's Theorem
 (with ([x . y] pos) (with ([dx . dy] (vector-ref wind-xy wind)) (- (* x dy) (* y dx)))))
;; Use Green's Theorem to get the area; it's signed depending on which way you turned, so use abs.
;; Then remove the space used by the pipes themselves, and add 1 for a dimension 0 fudge term.
(def (day10.2 pipes)
  (1+ (half (- (abs (day10* pipes green-darea)) (day10* pipes (lambda (_ _) 1))))))
;; Day 10 Wrap up
(def day10-example1 "\
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ")
(def day10-example2.1 "\
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")
(def day10-example2.2 "\
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L")
(def (day10 (input (day-input-string 10)))
  (def example1 (parse-2d-string day10-example1))
  (check (day10.1 example1) => 8)
  (def example2.1 (parse-2d-string day10-example2.1))
  (check (day10.2 example2.1) => 4)
  (def example2.2 (parse-2d-string day10-example2.2))
  (check (day10.2 example2.2) => 10)
  (def pipes (parse-2d-string input))
  [(day10.1 pipes) (day10.2 pipes)])


;;; DAY 11 https://adventofcode.com/2023/day/11 -- Green's Theorem
;; Day 11 Part 1
(def (sumdistances widths coords)
  (def L (vector-length widths))
  (def conv (make-vector L 0))
  (for (i (in-range 1 L))
    (set! (vector-ref conv i) (+ (vector-ref conv (1- i)) (vector-ref widths (1- i)))))
  (def sc (sort (map (lambda (c) (vector-ref conv c)) coords) <))
  (def N (length sc))
  (def sum 0)
  (for ((i (in-range N))
        (x sc))
    (increment! sum (* x (- (* 2 i) N -1))))
  sum)
(def (day11* galaxy age)
  (with ((vector X Y s) galaxy)
    (let ((xwidth (make-vector X age))
          (ywidth (make-vector Y age))
          (stars '()))
      (for (x (in-range X))
        (for (y (in-range Y))
          (let (c (xyget galaxy x y))
            (when (eqv? c #\#)
              (set! (vector-ref xwidth x) 1)
              (set! (vector-ref ywidth y) 1)
              (push! (cons x y) stars)))))
      (+ (sumdistances xwidth (map car stars))
         (sumdistances ywidth (map cdr stars))))))
(def (day11.1 galaxy) (day11* galaxy 2))
(def (day11.2 galaxy) (day11* galaxy 1000000))
;; Day 11 Wrap up
(def day11-example "\
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")
(def (day11 (input (day-input-string 11)))
  (def example (parse-2d-string day11-example))
  (check (day11.1 example) => 374)
  (check (day11* example 10) => 1030)
  (check (day11* example 100) => 8410)
  (def pipes (parse-2d-string input))
  [(day11.1 pipes) (day11.2 pipes)])
