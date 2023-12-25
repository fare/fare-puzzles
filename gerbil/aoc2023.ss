;; Solutions to https://AdventOfCode.com/2023 -*- Gerbil -*-
(export #t)
(import
  (for-syntax :std/iter :std/misc/number)
  (only-in :gerbil/gambit time integer-sqrt
           bit-count first-set-bit fxfirst-set-bit extract-bit-field bit-set?)
  (only-in :std/assert assert!)
  (only-in :std/debug/DBG DBG)
  (only-in :std/error check-argument)
  (only-in :std/format format)
  (only-in :std/iter for for/collect for/fold in-range)
  (only-in :std/misc/alist asetq aremq)
  (only-in :std/misc/dag walk-dag)
  (only-in :std/misc/evector
           list->evector evector->list evector->vector
           evector-ref evector-push! evector-fill-pointer
           ebits->bits bits->ebits ebits-ref ebits-set! memoize-recursive-sequence)
  (only-in :std/misc/func compose repeat)
  (only-in :std/misc/hash hash-ensure-ref hash-ensure-modify! invert-hash<-vector)
  (only-in :std/misc/list push! length=n? length>n? when/list butlast)
  (only-in :std/misc/list-builder with-list-builder)
  (only-in :std/misc/number increment! post-increment! decrement! half bezout
           check-argument-uint check-argument-positive-integer uint-of-length? xmin/list)
  (only-in :std/misc/path subpath)
  (only-in :std/misc/ports writeln with-output read-file-string)
  (only-in :std/misc/pqueue make-pqueue pqueue-empty? pqueue-push! pqueue-pop!)
  (only-in :std/misc/queue make-queue enqueue! dequeue! queue-empty?)
  (only-in :std/misc/vector vector-least-index)
  (only-in :std/parser/ll1 ll1/string ll1-begin ll1-begin0 ll1-bind ll1-pure ll1-result
           ll1-char ll1-char* ll1-char+ ll1-string ll1-skip-space* ll1-n-chars
           ll1-repeated ll1-separated ll1-lines ll1-eof ll1-eol ll1-eolf ll1-eolf?
           ll1-list ll1* ll1-uint ll1-sint ll1-case ll1-or ll1-peek)
  (only-in :std/sort sort)
  (only-in :std/source this-source-directory)
  (only-in :std/srfi/1 reduce concatenate split-at delete partition first second
           take take-while span append-map lset-intersection)
  (only-in :std/srfi/13 string-trim-right string-trim-both string-count string-delete
           string-index-right string-index string-reverse string-suffix? string-prefix?
           string-filter) ; strings
  (only-in :std/srfi/14 char-set? char-set-contains?) ; char-set
  (only-in :std/srfi/133 vector-count vector-fold vector-index) ; vectors
  (only-in :std/srfi/141 floor-quotient floor/ ceiling-quotient) ; integer division
  (only-in :std/sugar defrule if-let hash until ignore-errors)
  (only-in :std/test check)
  (only-in :std/text/char-set char-ascii-alphabetic? char-ascii-alphanumeric?
           codepoint-ascii-digit char-ascii-digit digit-char)
  (only-in :std/values first-value list->values vector->values)
  (only-in :clan/astar A* A*2)
  (only-in :clan/base nest !>)
  (only-in :clan/memo define-memo-function)
  (only-in :clan/order lexicographic<?)
  (only-in :clan/pure/dict/symdict empty-symdict symdict-has-key? symdict-put))

;;; General purpose utilities
(def (day-input-file n) (subpath (this-source-directory) (format "data/aoc2023-~d.input" n)))
(def (day-input-string n) (read-file-string (day-input-file n)))

(def u8vector-ref-set! u8vector-set!)
(def vector-ref-set! vector-set!)
(def string-ref-set! string-set!)
(def (+/list l) (foldl + 0 l))
(def (*vector k v) (vector-map (cut * k <>) v))
(def (vector+ . vs) (apply vector-map + vs))
(def (vector- . vs) (apply vector-map - vs))
(defrule (pushnew! element list opts ...)
  (unless (member* element list opts ...)
    (set! list (cons element list))))
(def (member* element list key: (key #f) test: (test #f) test-not: (test-not #f))
  (if (or key test test-not)
    (let* ((test (or test (and test-not (lambda (x y) (not (test-not x y)))) equal?))
           (key (or key identity))
           (pred (lambda (x) (test element (key x)))))
      (let loop ((l list)) (match l ([] #f) ([h . t] (if (pred h) l (loop t))))))
    (member element list)))
(def (u8vector-index u8v criterion (start 0) (end (u8vector-length u8v)))
  (cond
   ((uint-of-length? criterion 8)
    (let lp ((i start))
      (and (< i end)
           (if (= criterion (u8vector-ref u8v i)) i
               (lp (+ i 1))))))
   ((char? criterion)
    (u8vector-index u8v (char->integer criterion) start end))
   ((char-set? criterion)
    (let lp ((i start))
      (and (< i end)
           (alet (c (ignore-errors (integer->char criterion)))
             (if (char-set-contains? c (u8vector-ref u8v i)) i
                 (lp (+ i 1)))))))
   ((procedure? criterion)
    (let lp ((i start))
      (and (< i end)
           (if (criterion (u8vector-ref u8v i)) i
               (lp (+ i 1))))))
   (else
    (error "Second param is neither byte, char-set, char, or predicate procedure."
      u8vector-index criterion))))
(defrule (fxbit-clear bit bitmask) (##fxandc2 bitmask (fxshift 1 bit)))
(defrule (fxbit-set bit bitmask) (##fxior bitmask (fxshift 1 bit)))
(def (bit-clear bit bitmask) (##bit-field-clear bitmask bit (1+ bit)))
(def (bit-set bit bitmask) (##bit-field-set bitmask bit (1+ bit)))

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
(def (parse-2d-map string) ;; : String -> (Vector-Tuple UInt UInt Bytes)
  (2d-map<-rows (ll1/string ll1-lines string)))
(def (make-2d-map X Y (V (make-u8vector (* X Y) 0))) (vector X Y V))
(def (2d-map<-rows rows) ;; : (List String) -> (Vector-Tuple UInt UInt String)
  (def n-rows (length rows))
  (check-argument-positive-integer n-rows)
  (def n-cols (string-length (first rows)))
  (check-argument-positive-integer n-cols)
  (check-argument (andmap (lambda (row) (equal? (string-length row) n-cols)) rows)
                  "equal length rows" string)
  (make-2d-map n-cols n-rows (u8vector-concatenate (map string->bytes rows))))
(defrule (i<-xy x y X Y) (+ x (* y X)))
(defrule (xy<-i i X Y) (let-values (((y x) (floor/ i X))) (values x y)))
(defrule (xy? v x y)
  (with ((vector X Y V) v)
    (and (exact-integer? x) (< -1 x X) (exact-integer? y) (< -1 y Y))))
(defrule (i<-p p X Y) (i<-xy (car p) (cdr p) X Y))
(defrule (p<-i i X Y) (let-values (((x y) (xy<-i i X Y))) (cons x y)))
(defrule (xyp? v p) (with ([x . y] p) (xy? v x y)))
(defrule (xyget v x y)
  (and (xy? v x y) (with ((vector X Y V) v) (u8vector-ref V (i<-xy x y X Y)))))
(defrule (xygetc v x y) (alet (i (xyget v x y)) (integer->char i)))
(defrule (pget v p) (xyget v (car p) (cdr p)))
(defrule (pgetc v p) (xygetc v (car p) (cdr p)))
(def (xycopy m) (with ((vector X Y V) m) (vector X Y (u8vector-copy V))))
;; Day 3 Iterators -- in lieu of a data structure
(def (d3-for-each-number fun v)
  (with ((vector X Y V) v)
    (for ((y (in-range Y)))
      (let loop ((xstart 0))
        (when (< xstart X)
          (if (char-ascii-digit (xygetc v xstart y))
            (let* ((xendy (u8vector-index V (lambda (x) (not (codepoint-ascii-digit x)))
                                          (1+ (i<-xy xstart y X Y)) (i<-xy X y X Y)))
                   (xend (if xendy (- xendy (i<-xy 0 y X Y)) X))
                   (n (string->number (bytes->string (subu8vector V (i<-xy xstart y X Y) (i<-xy xend y X Y))))))
              (fun n xstart xend y)
              (loop (1+ xend)))
            (loop (1+ xstart))))))))
(def (part-symbol? x)
  (and (char? x) (not (or (eqv? x #\.) (char-ascii-digit x)))))
(def (d3-for-each-adjacent-symbol fun v xstart xend y)
  (with ((vector X Y _) v)
    (def (c gx gy)
      (let (g (xygetc v gx gy))
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
  (def v (parse-2d-map input))
  (def sum 0)
  (def (process-num n xstart xend y)
    (when (has-adjacent-symbol? v xstart xend y)
      (increment! sum n)))
  (d3-for-each-number process-num v)
  sum)
;; Day 3 Part 2
(def (day3.2 input)
  (def v (parse-2d-map input))
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
(def (d5m-shift s m) ;; x -> m( x + s )
  (if (= s +inf.0) []
      (map (match <> ([src . off] [(- src s) :: (+ off s)])) m)))
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
(def (*xy k xy) (with ([x . y] xy) (cons (* k x) (* k y))))
(def wind-names #(North South East West)) ;; 0 1 2 3
(def wind-chars "NSEW") ;; 0 1 2 3
(def (char->wind c) (string-index wind-chars c))
(defrules for-each-wind ()
  ((_ (w wbits) body ...)
   (let (f (lambda (w) (when (fxbit-set? w wbits) body ...)))
     (f 0) (f 1) (f 2) (f 3)))
  ((_ (w) body ...)
   (let (f (lambda (w) body ...)) (f 0) (f 1) (f 2) (f 3))))
(def (winds->bits ws) (+/list (map (cut fxshift 1 <>) ws)))
(def (bits->winds b) (filter (cut fxbit-set? <> b) (iota 4)))
(def wind-xy #((0 . -1) (0 . 1) (1 . 0) (-1 . 0)))
(def (wind->xy w (n 1)) (*xy n (vector-ref wind-xy w)))
(def (-wind wind) (bitwise-xor wind 1))
(def (pos+wind pos wind (n 1)) (xy+ pos (wind->xy wind n)))
(def pipe-chars "|-LJ7F.S")
(def pipe-connections #((0 1) (2 3) (0 2) (0 3) (1 3) (1 2) () (0 1 2 3)))
(def (pipe-get v p) (alet (c (pgetc v p)) (string-index pipe-chars c)))
(def (has-connection? v pos wind)
  (alet (pipe (pipe-get v pos))
    (member wind (vector-ref pipe-connections pipe))))
(def (find-wind v p winds)
  (find (lambda (w) (has-connection? v (pos+wind p w) (-wind w))) winds))
(def (day10* v f)
  (with ((vector X Y s) v)
    (defvalues (x y) (xy<-i (u8vector-index s (char->integer #\S)) X Y))
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
(def (green-darea pos wind (n 1)) ;; applying Green's Theorem
 (with ([x . y] pos) (with ([dx . dy] (wind->xy wind n)) (- (* x dy) (* y dx)))))
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
  (def example1 (parse-2d-map day10-example1))
  (check (day10.1 example1) => 8)
  (def example2.1 (parse-2d-map day10-example2.1))
  (check (day10.2 example2.1) => 4)
  (def example2.2 (parse-2d-map day10-example2.2))
  (check (day10.2 example2.2) => 10)
  (def pipes (parse-2d-map input))
  [(day10.1 pipes) (day10.2 pipes)])


;;; DAY 11 https://adventofcode.com/2023/day/11 -- L¹ distance and counting
(def (sumdistances widths coords)
  (def L (vector-length widths))
  (def conv (make-vector L 0))
  (for (i (in-range 1 L))
    (set! (vector-ref conv i) (+ (vector-ref conv (1- i)) (vector-ref widths (1- i)))))
  ;; Instead of summing N(N-1)/2 differences,
  ;; sort the terms in N*log N then count how much each term appears positively vs negatively
  (def sc (sort (map (lambda (c) (vector-ref conv c)) coords) <))
  (def N (length sc))
  (foldl (lambda (x i s) (+ s (* x (- (* 2 i) N -1)))) 0 sc (iota N)))
(def (day11* galaxy (age 2))
  (with ((vector X Y s) galaxy)
    (let ((xwidth (make-vector X age))
          (ywidth (make-vector Y age))
          (stars '()))
      (for (x (in-range X))
        (for (y (in-range Y))
          (let (c (xygetc galaxy x y))
            (when (eqv? c #\#)
              (set! (vector-ref xwidth x) 1)
              (set! (vector-ref ywidth y) 1)
              (push! (cons x y) stars)))))
      (+ (sumdistances xwidth (map car stars)) ;; L¹ means we can sum the coordinates independently
         (sumdistances ywidth (map cdr stars))))))
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
  (def example (parse-2d-map day11-example))
  (check (day11* example) => 374)
  (check (day11* example 10) => 1030)
  (check (day11* example 100) => 8410)
  (def pipes (parse-2d-map input))
  [(day11* pipes) (day11* pipes 1000000)])


;;; DAY 12 https://adventofcode.com/2023/day/12 -- search within constraints
;; Heuristic optimizations:
;; 1. cache previous searches (see the search-cache)
;; 2. simplify away known complexities (initial, final or consecutive dots)
;; 3. shortcut for failure/impossibility/0 (detect it, have it simplify away other subsearches)
;; 4. optimize easy cases
;; 5. heuristic: prioritize lowest entropy choices (subsearch with fewest cases, longest sequence of #)
(def ll1-d12l
  (ll1* cons (ll1-begin0 (ll1-char* "?#.") (ll1-char #\space))
        (ll1-separated ll1-uint (ll1-char #\,) ll1-eolf?)))
(def parse-d12 (cut ll1/string (cut ll1-lines <> ll1-d12l) <>))
(def (has-consecutive-dots? line)
  (let loop ((i (string-length line)))
    (and (fx>0? i)
         (let (j (string-index-right line #\. 0 i))
           (and (fx>0? j)
                (or (eqv? #\. (string-ref line (fx1- j)))
                    (loop (- j 2))))))))
(def (simplify-consecutive-dots line)
  (if (has-consecutive-dots? line)
    (with-output (o #f)
      (def l (string-length line))
      (let loop ((i 0))
        (when (and i (< i l))
          (let (j (string-index line #\. i))
            (write-substring line i (if j (1+ j) l) o)
            (when j (let (k (fx1+ j))
                      (when (< k l) (loop (string-index line (lambda (x) (not (eqv? x #\.))) k)))))))))
    line))
(def (combinations n k) (/ (fact n) (* (fact k) (fact (- n k)))))
(defstruct search-node
  (result ;; result if done or #f. No reentrancy allowed (or we'd have a separate field)
   entropy ;; entropy estimate. How much branching over this node is estimated to blow up search.
   state) ;; state of the search
  transparent: #t)
(def (search-done result search) (search-node result (if (zero? result) -1 0) search))
(def (count-huhs s) (string-count s #\?))
(def search-cache (hash))
(def (cached-nodify search) (hash-ensure-ref search-cache search (cut search-nodify search)))
(def (cached-node entropy search)
  (hash-ensure-ref search-cache search (cut search-node #f entropy search)))
(def (simplify-pattern p) (simplify-consecutive-dots (string-trim-both p #\.)))
(def (wanted-length* wanted) ;; total length of #'s + .'s wanted in line + 1 extra for . if not null
  (+ (length wanted) (+/list wanted)))
(def (wanted-length wanted) ;; total length of #'s + .'s wanted in line
  (if (null? wanted) 0 (1- (wanted-length* wanted))))
(def (search-nodify search) ;; search-node <- search
  (with ([s . c] search)
    (let* ((s (string-trim-both s #\.))
           (l (string-length s)))
      (cond
       ((null? c) (search-done (if (string-index s #\#) 0 1) search))
       ((zero? l) (search-done 0 search))
       ((null? (cdr c))
        (let (k (car c))
          (cond ((> k l) (search-done 0 search))
                ((= k l) (search-done (if (string-index s #\.) 0 1) search))
                (else (cached-node (- l k) [s . c])))))
       (else
        (let* ((ls (wanted-length c))
               (f (min (- l ls) (count-huhs s))))
          (cond
           ((> ls l) (search-done 0 search))
           (else (cached-node (combinations f (min f (length c))) [s . c])))))))))
(def (find-best-choice entropy choices)
  (with ([c . r] choices)
    (let loop ((min (entropy c)) (best 0) (i 1) (unseen r))
      (match unseen ([] best)
             ([a . r] (let (e (entropy a))
                        (if (< e min) (loop e i (1+ i) r) (loop min best (1+ i) r))))))))
(def (extract-best-choice entropy choices)
  (call-with-values (cut split-at choices (find-best-choice entropy choices)) append))
(def (d12-split s start end before after)
  (let (l (string-length s))
    (if (or (and (> start 0) (eqv? #\# (string-ref s (1- start))))
            (and (< end l) (eqv? #\# (string-ref s end)))
            (string-index s #\. start end))
      0
      (d12-search/count 1
        [(cached-nodify [(substring s 0 (max 0 (1- start))) . before])
         (cached-nodify [(substring s (min (1+ end) l) l) . after])]))))
(def (d12-resolve n)
  (with ([s . c] (search-node-state n))
    (def l (string-length s))
    (cond
     ((eqv? #\# (string-ref s 0)) (d12-split s 0 (car c) [] (cdr c)))
     ((eqv? #\# (string-ref s (1- l))) (d12-split s (- l (last c)) l (butlast c) []))
     (else (let* ((best (find-best-choice - c))
                  ((values before after*) (split-at c best))
                  (k (car after*))
                  (after (cdr after*))
                  (start (wanted-length* before))
                  (end (- l k (wanted-length* after) -1)))
             (if (>= start end)
               0
               (+/list (for/collect ((i (in-range start end)))
                         (d12-split s i (+ i k) before after)))))))))
(def (d12-examine n) ;; UInt <- SearchNode
  (or (search-node-result n)
      (let (r (d12-resolve n))
        (set! (search-node-result n) r)
        (set! (search-node-entropy n) (if (zero? r) -1 0))
        r)))
(def (d12-search/count factor subsearches) ;; UInt <- UInt (List
  (if (zero? factor)
    0
    (match subsearches
      ([] factor)
      (_ (with ([c . r] (extract-best-choice search-node-entropy subsearches))
           (d12-search/count (* factor (d12-examine c)) r))))))
(def (d12-possibilities line)
  (with ([s . c] line) (d12-search/count 1 [(cached-nodify [(simplify-pattern s) . c])])))
(def (day12.1 lines)
  (+/list (map d12-possibilities lines)))
(def (quintuplicate line)
  (with ([s . c] line) (cons (string-join (repeat s 5) "?") (concatenate (repeat c 5)))))
(def (day12.2 lines)
  (day12.1 (map quintuplicate lines)))
;; Day 12 Wrap up
(def day12-example "\
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")
(def (day12 (input (day-input-string 12)))
  (def example (parse-d12 day12-example))
  (check (day12.1 example) => 21)
  (check (day12.2 example) => 525152)
  (def lines (parse-d12 input))
  [(day12.1 lines) (day12.2 lines)]) ;; takes ~5s on my laptop using the interpreter

;;; DAY 13 https://adventofcode.com/2023/day/13 -- off-by-one errors
;; “There are two hard things in computer science: cache invalidation, naming things, and off-by-one errors.”
(def ll1-d13 (ll1-repeated
              (ll1-repeated (ll1-begin0 (ll1-char+ ".#") ll1-eolf) ll1-eolf)
              ll1-eof))
(def (d13-parse string)
  (map 2d-map<-rows (ll1/string ll1-d13 string)))
(def (xyset! v x y b)
  (with ((vector X Y V) v)
    (check-argument (and (exact-integer? x) (< -1 x X)) "in range" [x X])
    (check-argument (and (exact-integer? y) (< -1 y Y)) "in range" [y Y])
    (def i (i<-xy x y X Y))
    (check-argument (< -1 i (* X Y)) "in range" [i (* X Y)])
    (check-argument (< -1 b 256) "u8" b)
    (set! (u8vector-ref V i) b)))
(def (xysetc! v x y c) (xyset! v x y (char->integer c)))
(def (pset! v p b) (xyset! v (car p) (cdr p) b))
(def (psetc! v p c) (pset! v p (char->integer c)))
(def xygetc-set! xysetc!)
(def xyget-set! xyset!)
(def pget-set! pset!)
(def pgetc-set! psetc!)
(def (transpose-2d-map v)
  (with ((vector X Y s) v)
    (def w (make-2d-map Y X))
    (for (x (in-range X)) (for (y (in-range Y)) (set! (xygetc w y x) (xygetc v x y))))
    w))
(def (vertical-symmetry? r m (smudges 0))
  (with ((vector X Y _) m)
    (let/cc return
      (unless (<= 1 r (- X 1)) (return #f))
      (for (x (in-range (min r (- X r))))
        (for (y (in-range Y))
          (unless (eqv? (xygetc m (- r x 1) y) (xygetc m (+ r x) y))
            (decrement! smudges)
            (when (negative? smudges) (return #f)))))
      (zero? smudges))))
(def (summarize-vertical-symmetry m (smudges 0))
  (with ((vector X Y _) m)
    (let/cc return
      (def (t r) (when (vertical-symmetry? r m smudges) (return r)))
      (for (r (in-range 1 X)) (t r)) ;; start from top/left
      0)))
(def (summarize-symmetry m (smudges 0))
  (let (x (summarize-vertical-symmetry m smudges))
    (if (positive? x) x
        (* 100 (summarize-vertical-symmetry (transpose-2d-map m) smudges)))))
(def (day13.1 maps)
  (+/list (map summarize-symmetry maps)))
(def (day13.2 maps)
  (+/list (map (cut summarize-symmetry <> 1) maps)))
(def day13-example "\
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")
(def (day13 (input (day-input-string 13)))
  (def example (d13-parse day13-example))
  (check (day13.1 example) => 405)
  (check (day13.2 example) => 400)
  (def maps (d13-parse input))
  [(day13.1 maps) (day13.2 maps)]) ;; 36041 35915


;;; DAY 14 https://adventofcode.com/2023/day/14 -- detecting cycles
;; This is quite redundant with DAY 8 and DAY 13.
;; This solution triggers segfaults in the Gerbil interpreter,
;; no problem with the Gerbil compiler or with the Gambit interpreter.
;; adding DBG statements, (##gc) forms, and using export GAMBOPT="d5,te"
;; I eventually reached i=102 (j=93, k=7) without segfault even with the interpreter.
;; Still very worrisome.
(def (tiltN m)
  (with ((vector X Y s) m)
    (def n (vector X Y (u8vector-copy s)))
    (for (x (in-range X))
      (for (y (in-range (1- Y)))
        (when (eqv? #\. (xygetc n x y))
          (let/cc return
            (for (z (in-range (1+ y) Y))
              (case (xygetc n x z)
                ((#\O) (xysetc! n x y #\O) (xysetc! n x z #\.) (return))
                ((#\#) (return))))))))
    n))
(def (loadN m)
  (with ((vector X Y s) m)
    (def l 0)
    (for (x (in-range X))
      (for (y (in-range Y))
        (when (eqv? #\O (xygetc m x y))
          (increment! l (- Y y)))))
    l))
(def (rotate-clockwise m)
  (with ((vector X Y s) m)
    (def n (make-2d-map Y X))
    (for (x (in-range X)) (for (y (in-range Y)) (xysetc! n (- Y y 1) x (xygetc m x y))))
    n))
(def (cycle14 m)
  (def (f m) (begin0 (rotate-clockwise (tiltN m))))
  (f (f (f (f m)))))
(def (iterate n f x)
  (if (zero? n) x (iterate (1- n) f (f x))))
(def (day14.1 m)
  (loadN (tiltN m)))
(def d14h (hash))
(def (day14.2 m)
  (set! d14h (hash))
  (def h d14h) ;; (hash)) ;;(def v (list->evector '()))
  (def N 1000000000)
  (let/cc return
    (let loop ((i 0) (m m))
      (if (= i N)
        (loadN m)
        (let (j (hash-get h m));)
          (if j (let (k (modulo (- N i) (- i j)))
                  (for-each display ["  day14.2: " i " " j " " k])
                  (loadN (iterate (modulo (- N i) (- i j)) cycle14 m)))
              (begin
                (hash-put! h m i)
                (for-each display [" " i])
                (loop (1+ i) (cycle14 m)))))))))
(def (print-2d-map m (port (current-output-port)))
  (with ((vector X Y s) m)
    (for (y (in-range Y))
      (let* ((start (i<-xy 0 y X Y)) (end (+ start X)))
        (write-subu8vector s start end port) (newline port)))))
(def day14-example "\
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")
(def day14-example.N "\
OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#....")
(def day14-example.1 "\
.....#....
....#...O#
...OO##...
.OO#......
.....OOO#.
.O#...O#.#
....O#....
......OOOO
#...O###..
#..OO#....")
(def day14-example.2 "\
.....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#..OO###..
#.OOO#...O")
(def day14-example.3 "\
.....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#...O###.O
#.OOO#...O")
(def (day14 (input (day-input-string 14)))
  (def example (parse-2d-map day14-example))
  (def example.N (parse-2d-map day14-example.N))
  (check (tiltN example) => example.N)
  (check (day14.1 example) => 136)
  (def example.1 (parse-2d-map day14-example.1))
  (def example.2 (parse-2d-map day14-example.2))
  (def example.3 (parse-2d-map day14-example.3))
  (check (cycle14 example) => example.1)
  (check (cycle14 example.1) => example.2)
  (check (cycle14 example.2) => example.3)
  (check (day14.2 example) => 64)
  (def m (parse-2d-map input))
  [(day14.1 m) (day14.2 m)]) ;; 107430 96317

;;; DAY 15 https://adventofcode.com/2023/day/15 -- simple 1960s data processing
(def (h15-u8 byte current-value)
  (bitwise-and 255 (* (+ current-value byte) 17)))
(def (h15-u8vector u8v (current-value 0))
  (for (i (in-range (u8vector-length u8v)))
    (set! current-value (h15-u8 (u8vector-ref u8v i) current-value)))
  current-value)
(def (h15-string string (current-value 0)) (h15-u8vector (string->bytes string)))
(def (d15.1 initseq)
  (+/list (map h15-string initseq)))
(def (d15-parse string) (string-split (string-delete #\newline string) #\,))
(def (d15-parse-step step)
  (let* ((i (string-index step (lambda (x) (not (char-ascii-alphabetic? x)))))
         (label (substring step 0 i))
         (operation (string-ref step i)) ;; = or -
         (power (and (eqv? operation #\=)
                     (string->number (substring step (1+ i) (string-length step))))))
    (values label power)))
(def (d15-step step boxes)
  (defvalues (label power) (d15-parse-step step))
  (def h (h15-string label))
  (def sym (string->symbol label))
  (def old (vector-ref boxes h))
  (set! (vector-ref boxes h) (if power (asetq old sym power) (aremq sym old))))
(def (d15-init initseq)
  (let (boxes (make-vector 256 '())) (for-each (cut d15-step <> boxes) initseq) boxes))
(def (d15-box-power i box)
  (let (l (length box)) (* (1+ i) (+/list (map * (map cdr box) (iota l l -1))))))
(def (d15-power boxes) (+/list (map (lambda (i) (d15-box-power i (vector-ref boxes i))) (iota 256))))
(def (d15.2 initseq) (d15-power (d15-init initseq)))
(def d15-example "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
(def d15-boxes
  (let (i '(((rn . 1) (cm . 2)) () () ((ot . 7) (ab . 5) (pc . 6))))
    (list->vector (append (map reverse i) (repeat '() 252)))))
(def (day15 (input (day-input-string 15)))
  (def example (d15-parse d15-example))
  (check (d15.1 example) => 1320)
  (check (d15-init example) => d15-boxes)
  (check (d15.2 example) => 145)
  (def is (d15-parse input))
  [(d15.1 is) (d15.2 is)]) ;; 512283 (too high 512293 (forgot to remove newline))


;;; DAY 16 https://adventofcode.com/2023/day/16 -- warm up for Day 17
(def d16chars "./\\-|")
(def d16table
  (list->vector
    (for/collect ((ws '("NS SN EW WE" "NW SE ES WN" "NE SW EN WS" "NEW SEW EW WE" "NS SN ENS WNS")))
      (for/fold (r 0) ((wl (string-split ws #\space)))
        (with ([i . os] (map char->wind (string->list wl)))
          (+ r (fxshift (winds->bits os) (fxshift i 2))))))))
(def (d16next wind char)
  (def charnexts (vector-ref d16table (string-index d16chars char)))
  (fxbit-clear wind (extract-bit-field 4 (fxshift wind 2) charnexts)))
(def (d16step m head lit c)
  (with ((vector X Y _) m)
    (with ([w x . y] head)
      (unless (fxbit-set? w (xyget lit x y))
        (let ((windmask (fxshift 1 w)))
          (def (go wind)
            (unless (fxbit-set? wind (xyget lit x y))
              (let (nextpos (pos+wind [x . y] wind))
                (when (xyp? m nextpos)
                  (c (cons (-wind wind) nextpos))))
              (set! windmask (fxbit-set wind windmask))))
          ;;(for-each-wind (next (d16next w (xygetc m x y))) (go next))
          (for-each go (bits->winds (d16next w (xygetc m x y))))
          (xyset! lit x y (bitwise-ior (xyget lit x y) windmask)))))))
(def (d16* m init)
  (with ((vector X Y s) m)
    (def lit (make-2d-map X Y))
    (def t (vector-ref lit 2))
    (def (litN) (for/fold (sum 0) ((i (iota (* X Y))))
                  (if (zero? (u8vector-ref t i)) sum (1+ sum))))
    (let loop ((heads [init]))
      (if (null? heads) (litN)
          (loop (with-list-builder (c) (for-each (cut d16step m <> lit c) heads)))))))
(def (d16.1 m) (d16* m '(3 0 . 0))) ;; entering the first square from the West
(def (d16.2 m)
  (with ((vector X Y _) m)
    (def maxX (1- X))
    (def maxY (1- Y))
    (def inits
      (append (for/collect (x (iota X)) [0 x . 0])
              (for/collect (x (iota X)) [1 x . maxY])
              (for/collect (y (iota Y)) [2 maxX . y])
              (for/collect (y (iota Y)) [3 0 . y])))
    (foldl max 0 (map (cut d16* m <>) inits))))
(def d16-ex "\
.|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")
(def (day16 (input (day-input-string 16)))
  (def ex (parse-2d-map d16-ex))
  (check (d16.1 ex) => 46)
  (check (d16.2 ex) => 51)
  (def m (parse-2d-map input))
  [(d16.1 m) (d16.2 m)])

;;; DAY 17 https://adventofcode.com/2023/day/17 -- A* search (best first search)
;;; Phase space is (direction . position)
(def (right-turn wind) (u8vector-ref #u8(2 3 1 0) wind))
(def (d17arcs m minsteps maxsteps forward? state)
  (match state
    ([wind . pos]
     (let* ((r (right-turn wind)) (l (-wind r)))
       (with-list-builder (c)
         (def (go w)
           (def d (if forward? w (-wind wind)))
           (let loop ((cost 0) (pos pos) (steps 1))
             (when (<= steps maxsteps)
               (let (p (pos+wind pos d))
                 (when (xyp? m p)
                   (let (cost (+ cost (char-ascii-digit (pgetc m (if forward? p pos)))))
                     (when (>= steps minsteps)
                       (c [cost w . p]))
                     (loop cost p (1+ steps))))))))
         (go r) (go l))))))
(def (d17* m minsteps maxsteps)
  (with ((vector X Y _) m)
    (car (A*2 starts: '((0 (0 0 . 0)) ;; cost 0 so far, heading North, at NW corner.
                        (0 (3 0 . 0))) ;; cost 0 so far, heading West, at NW corner.
              ends: `((0 (1 ,(1- X) . ,(1- Y))) ;; cost 0 so far, heading South, at SE corner.
                      (0 (2 ,(1- X) . ,(1- Y)))) ;; cost 0 so far, heading East, at SE corner.
              +arcs: (cut d17arcs m minsteps maxsteps #t <>)
              -arcs: (cut d17arcs m minsteps maxsteps #f <>)))))
(def (d17.1 m) (d17* m 1 3))
(def (d17.2 m) (d17* m 4 10))
(def d17ex1 "\
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533")
(def d17ex2 "\
111111111111
999999999991
999999999991
999999999991
999999999991")
(def (day17 (input (day-input-string 17)))
  (def ex1 (parse-2d-map d17ex1))
  (check (d17.1 ex1) => 102)
  (check (d17.2 ex1) => 94)
  (def ex2 (parse-2d-map d17ex2))
  (check (d17.2 ex2) => 71)
  (def m (parse-2d-map input))
  [(d17.1 m) (d17.2 m)]) ;; 959 1135

;;; DAY 18 https://adventofcode.com/2023/day/18 -- Green's Theorem, again (see Day 10)
(def uind-chars "UDRL") ;; 0 1 2 3 ;; NSEW by any other name
(def ll1-d18line
  (ll1-list (ll1-begin0 (ll1-char uind-chars) (ll1-string " "))
            (ll1-begin0 ll1-uint (ll1-string " (#"))
            (ll1-begin0 (cut ll1-uint <> 16) (ll1-string ")") ll1-eolf)))
(def (d18parse m) (ll1/string (ll1-repeated ll1-d18line ll1-eof) m))
(def (d18dim dig)
  (let loop ((x 0) (y 0) (xmin 0) (xmax 0) (ymin 0) (ymax 0) (dig dig))
    (match dig
      ([] [xmin x xmax ymin y ymax])
      ([[uind n _] . dr]
       (with ([x1 . y1] (pos+wind [x . y] (string-index uind-chars uind) n))
         (loop x1 y1 (min x1 xmin) (max x1 xmax) (min y1 ymin) (max y1 ymax) dr))))))
(def (d18* dig decode)
  (let loop ((p [0 . 0]) (l 0) (a 0) (d (map decode dig)))
    (match d
      ([] (assert! (equal? p [0 . 0]))
       (1+ (half (+ (abs a) l))))
      ([[wind . n] . dr]
       (loop (pos+wind p wind n) (+ l n) (+ a (green-darea p wind n)) dr)))))
(def (d18.1 dig)
  (d18* dig (match <> ([uind n _] (cons (string-index uind-chars uind) n)))))
(def (d18.2 dig)
  (d18* dig (match <> ([_ _ x] (cons (vector-ref #(2 1 3 0) (fxand x 3)) (fxshift x -4))))))
(def d18ex1 "\
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")
(def (day18 (input (day-input-string 18)))
  (def ex1 (d18parse d18ex1))
  (check (d18.1 ex1) => 62)
  (check (d18.2 ex1) => 952408144115)
  (def dig (d18parse input))
  [(d18.1 dig) (d18.2 dig)])

;;; DAY 19 https://adventofcode.com/2023/day/19 -- Trivial Packet Routing Language Interpreter and Analysis
(def ll1-sym (ll1* make-symbol (ll1-char+ char-ascii-alphabetic?)))
(def (xmas->i c) (case c ((x) 0) ((m) 1) ((a) 2) ((s) 3)))
(def ll1-d19action
  (ll1* (lambda (sym more) (match more (#f sym)
                             ([cmp val rule] (list (xmas->i sym) cmp val rule))))
        ll1-sym
        (ll1-or (ll1-begin (ll1-peek #\}) (ll1-pure #f))
                (ll1-list (ll1* make-symbol (ll1-char "<>")) ll1-uint
                          (ll1-begin (ll1-string ":") ll1-sym)))))
(def ll1-d19rule
  (ll1* (lambda (name actions) (list name (butlast actions) (last actions)))
        (ll1-begin0 ll1-sym (ll1-string "{"))
        (ll1-separated ll1-d19action (ll1-string ",") (ll1-string "}\n"))))
(def ll1-d19pkt
  (ll1* vector (ll1-begin (ll1-string "{x=") ll1-uint)
        (ll1-begin (ll1-string ",m=") ll1-uint)
        (ll1-begin (ll1-string ",a=") ll1-uint)
        (ll1-begin (ll1-string ",s=") (ll1-begin0 ll1-uint (ll1-string "}") ll1-eolf))))
(def ll1-d19
  (ll1* cons (ll1* list->hash-table (ll1-repeated ll1-d19rule ll1-eol))
        (ll1-repeated ll1-d19pkt ll1-eof)))
(def (d19parse p) (ll1/string ll1-d19 p))
;; Day 19 Part 1
(def (comparator cmp) (case cmp ((<) <) ((>) >)))
(def (d19run rules pkt)
  (let runrule ((rule 'in))
    (case rule
      ((A) (+/list (vector->list pkt)))
      ((R) 0)
      (else (with ([actions fallback] (hash-get rules rule))
              (let runaction ((as actions))
                (match as ([] (runrule fallback))
                       ([[xmas cmp val r] . ar]
                        (if ((comparator cmp) (vector-ref pkt xmas) val)
                          (runrule r) (runaction ar))))))))))
(def (d19.1 rp)
  (with ([rules . pkts] rp) (+/list (map (cut d19run rules <>) pkts))))
;; Day 19 Part 2
(def (hc-size hypercube)
  (vector-fold * 1 (vector-map (cut apply - <>) hypercube)))
(def (hc-split xmas cmp val hc)
  (case cmp
    ((>) (let-values (((lo hi) (hc-split xmas '< (1+ val) hc))) (values hi lo)))
    ((<) (with ([start end] (vector-ref hc xmas))
           (cond ((<= val start) (values #f hc))
                 ((<= end val) (values hc #f))
                 (else (values (##vector-set hc xmas [start val])
                               (##vector-set hc xmas [val end]))))))))
(def (d19count rules hypercube)
  (def count 0)
  (let count/rule ((rule 'in) (hc hypercube) (path '()) (visited empty-symdict))
    #;(DBG dcr: rule hc path)
    (if (symdict-has-key? visited rule)
      (DBG d19-circularity: hc path) ;; are we being tricked with cases of non-termination?
      (case rule
        ((A) #;(DBG accept: hc path) (increment! count (hc-size hc)))
        ((R) #;(DBG reject: hc path) (void))
        (else
         (let ((path* (cons rule path)) (visited* (symdict-put visited rule #t)))
           (with ([actions fallback] (hash-get rules rule))
             (let count/action ((as actions) (hc hc))
               #;(DBG dca: rule as fallback hc path)
               (match as ([] (count/rule fallback hc path* visited*))
                      ([[xmas cmp val r] . ar]
                       (let-values (((yes no) (hc-split xmas cmp val hc)))
                         (when yes (count/rule r yes path* visited*))
                         (when no (count/action ar no))))))))))))
  count)
(def (d19.2 rp)
  (with ([rules . _] rp) (d19count rules (let (i [1 4001]) (vector i i i i)))))
(def d19ex "\
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}")
(def (day19 (input (day-input-string 19)))
  (def ex (d19parse d19ex))
  (check (d19.1 ex) => 19114)
  (check (d19.2 ex) => 167409079868000)
  (def rp (d19parse input))
  [(d19.1 rp) (d19.2 rp)])

;;; DAY 20 https://adventofcode.com/2023/day/20 -- Trivial Packet Routing Language Interpreter and Analysis
;; high is 0, low is 1.
;; % are flip flops. ignore high (0) inputs, flips on low (1)
;; (initially off (1), sending high output (0) on low input (1);
;; then on (0), sending low output (1) on low (1) input)
;; & are conjunctions, remembers each input, low initial state (1), after each pulse, issues NOR of updated state
;; broadcaster sends a copy of its input to all outputs
;; button sends low (1) to broadcaster
(def ll1-d20pattern
  (ll1-list (ll1-case ("%&" (ll1-char true)) (else (ll1-pure #\=))) ll1-sym))
(def ll1-d20module
  (ll1* cons ll1-d20pattern
        (ll1-begin (ll1-string " -> ")
                   (ll1-separated ll1-sym (ll1-string ", ") ll1-eolf))))
(def ll1-d20 (ll1-repeated ll1-d20module ll1-eof))
(def (d20parse p)
  (map (match <> ([[type module] . outs] [module type . outs]))
       (ll1/string ll1-d20 p)))
;; Day 20 Part 1
(defstruct module (name index type in-modules out-modules in-vars out-vars) transparent: #t)
(def (module-state-size m)
  (case (module-type m) ((#\=) 0) ((#\%) 1) ((#\&) (vector-length (module-in-modules m)))))
(def (d20prepare circuit)
  ;;(DBG 20000 circuit)
  (def h (list->hash-table circuit))
  (def c (list->evector '()))
  (def q (make-queue))
  (def by-name (hash))
  (def (name->index name) (if-let (m (hash-get by-name name)) (module-index m) -1))
  (def (register-module name)
    (alet (tos (or (hash-get h name)
                   (and (eq? name 'rx) '(#\=))))
      (with ([type . outputs] tos)
        (def i (evector-fill-pointer c))
        (def m (module name i type '() outputs #f #f))
        (evector-push! c m)
        (for-each (lambda (o) (enqueue! q [name . o])) outputs)
        m)))
  ;;(DBG 20100 h)
  (enqueue! q '(#f . broadcaster))
  (until (queue-empty? q)
    (with ([sender . recipient] (dequeue! q))
      (def m (hash-ensure-ref by-name recipient (cut register-module recipient)))
      (when (and sender m) (push! sender (module-in-modules m)))))
  (def ms (evector->vector c))
  ;;(DBG 20200 ms)
  (def v 0) ;; index of bits for state variables
  (vector-for-each (lambda (m)
                     (def i (module-index m))
                     (set! (module-in-modules m)
                       (list->vector (sort (map name->index (module-in-modules m)) <)))
                     (set! (module-in-vars m) (post-increment! v (module-state-size m)))
                     (set! (module-out-modules m)
                       (list->vector (map name->index (module-out-modules m))))
                     (for-each (lambda (j) (DBG circularity: 'sender (module-name (vector-ref ms j))
                                           'recipient (module-name m)))
                               (filter (cut > <> i) (vector->list (module-in-modules m)))))
                   ms)
  ;;(DBG 20300 ms)
  (vector-for-each (lambda (m)
                     (def i (module-index m))
                     ;;(DBG 20350 m)
                     (set! (module-out-vars m)
                       (vector-map
                        (lambda (o)
                          ;;(DBG 20355 o)
                          (if (= o -1) -1
                              (let (r (vector-ref ms o))
                                (if (eqv? (module-type r) #\&)
                                  (+ (module-in-vars r)
                                     (vector-least-index (cut <= i <>) (module-in-modules r)))
                                  -1))))
                        (module-out-modules m))))
                   ms)
  (def rx (alet (m (hash-get by-name 'rx)) (module-index m)))
  [v ms rx])
(def low-pulse 1)
(def high-pulse 0)
(def (low-pulse? hl) (= hl 1))
(def (high-pulse? hl) (= hl 0))
(def (pulse-not hl) (- 1 hl))
(def flipflop-off? low-pulse?)
(def flipflop-on? high-pulse?)
(def flipflop-off low-pulse)
(def flipflop-on high-pulse)
(def flipflop-toggle pulse-not)
(def (d20run ms state (counts (vector 0 0)) (watch #f))
  (def q (make-queue))
  (def (signal hl mm vv)
    (enqueue! q (vector hl mm vv))
    (increment! (vector-ref counts hl)))
  (signal low-pulse 0 -1) ;; push the button!
  (until (queue-empty? q)
    (with ((vector hl mi vi) (dequeue! q)) ;; hi/lo signal, module index, state var index (or -1)
      (when watch (watch hl mi))
      ;;(DBG r0: hl mi vi)
      (unless (= mi -1)
        (let (m (vector-ref ms mi))
          ;;(DBG r1: m)
          (def (send ss)
            (vector-for-each (cut signal ss <> <>)
                             (module-out-modules m) (module-out-vars m)))
          (case (module-type m)
            ((#\=) (send hl))
            ((#\%) (when (low-pulse? hl) ;; 1 is low
                     (let* ((v (module-in-vars m))
                            (old (ebits-ref state v)) ;; 1 is off
                            (new (flipflop-toggle old)))
                       (ebits-set! state v new)
                       (send (if (flipflop-off? old) high-pulse low-pulse)))))
             ((#\&) (ebits-set! state vi hl)
              (let (allhigh? (andmap (lambda (i) (high-pulse? (ebits-ref state i)))
                                     (iota (module-state-size m) (module-in-vars m))))
                (send (if allhigh? low-pulse high-pulse)))))))))
  ;;(newline)(show-state ms (first-value (ebits->bits state)))
  (cons counts state))
(def (show-state ms bits)
  ;;(DBG foo: bits)
  (def stateful (filter (lambda (m) (positive? (module-state-size m))) (vector->list ms)))
  (def last-i (module-index (last stateful)))
  (def names (map (compose as-string module-name) stateful))
  (def len (reduce max 0 (map string-length names)))
  (def v (foldl + 0 (map module-state-size stateful)))
  (for (i (iota len))
    (!> (with-output (o #f)
          (for-each (lambda (n m)
                      (display (if (< i (string-length n)) (string-ref n i) #\space) o)
                      (let (r (1- (module-state-size m)))
                        (when (positive? r)
                          (display (make-string r #\space) o))))
                    names stateful))
        (cut string-trim-right <> #\space)
        display)
    (newline))
  (for (j (iota v))
    (display (if (bit-set? j bits) #\- #\+)))
  (newline))
(def (d20run* circuit (N 1000))
  (with ([v ms _] (d20prepare circuit))
    ;;(DBG d20.1: v ms)
    (def state (bits->ebits -1 v)) ;; start all low / off
    (def counts (vector 0 0))
    (def countss (list->evector []))
    (def states (list->evector []))
    (def cache (hash))
    ;;(show-state ms -1)
    (let/cc return
      (for ((i (iota N 0)))
        (let (bits (first-value (ebits->bits state)))
          ;;(DBG d20run*A: i counts bits)
          (alet (j (hash-get cache bits))
            (let (k (+ (modulo (- N i) (- i j)) j))
              (return
               (cons
                (DBG d20run*X: i 'ic counts j 'jc (evector-ref countss j) k 'kc (evector-ref countss k) 'Nc
                (vector+ counts
                         (evector-ref countss k)
                         (*vector (floor-quotient (- N i) (- i j))
                                  (vector- counts (evector-ref countss j)))))
                     (evector-ref states k)))))
          (evector-push! countss (vector-copy counts))
          (evector-push! states bits)
          (hash-put! cache bits i)
          (d20run ms state counts)))
      (DBG d20run*F: counts)
      (cons counts (first-value (ebits->bits state))))))
(def (d20.1 circuit)
  (with ([(vector h l) . _] (d20run* circuit 1000)) (* h l)))
;; Day 20 Part 2
;; TODO: reduction to a DAG of strongly connected components,
;; then analysis of eventual periodicity in each component, using Day 8?
(def (d20.2 circuit)
  (with ([v ms rx] (d20prepare circuit))
    (DBG 20200 v ms rx)
    (void)))
;; Day 20 Redux
(def d20ex1 "\
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")
(def d20ex2 "\
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")
(def (day20 (input (day-input-string 20)))
  (def ex1 (d20parse d20ex1))
  (def ex2 (d20parse d20ex2))
  (check (d20.1 ex1) => 32000000)
  (check (d20.1 ex2) => 11687500)
  (def circuit (d20parse input))
  [(d20.1 circuit) (d20.2 circuit)]) ;; 617708918033885 too high


;;; DAY 21 https://adventofcode.com/2123/day/21 -- Distance
(def (d21parse p)
  (def m (parse-2d-map p))
  (with ((vector X Y V) m)
    (defvalues (x y) (xy<-i (string-index (bytes->string V) #\S) X Y))
    (xysetc! m x y #\.)
    (list x y m)))
;; DAY 21 Part 1 -- Easy
(def (d21.1 m (steps 64))
  (with ([x y v] m)
    (let loop ((i steps) (l [[x . y]]))
      (if (zero? i) (length l)
          (let ((u (xycopy v)))
            (loop (1- i) (with-list-builder (c)
                           (for (xy l)
                             (for-each-wind (w)
                               (let (p (pos+wind xy w))
                                 (when (eqv? (pgetc u p) #\.)
                                   (psetc! u p #\O)
                                   (c p))))))))))))
;; Day 21 Part 2 --
;; 1. Notice that all borders are clear. Akshully, also horizontal and vertical axes from S.
;;    This enables many simplifications (just rotate the puzzle to put it in [0 . 0])
;; 2. If a point is reachable in N-2 steps then it is reachable in N steps.
;;    If a point is reachable in N steps then it is reachable in N+2k steps for any natural k.
;; 3. If a point is outside the first square, the shortest path goes through the border;
;;    If the square is not aligned, a shortest goes through the incoming corner.
;;    If the axes are clear, if it's aligned a shortest path goes through the intersection
;;    of the axis and its square. If they aren't clear, extra complexity for aligned squares:
;;    have to find for each x and each y the shortest path across to each other x or y,
;;    and for big enough N the shortest path will go through a clear path most of the way;
;;    compute minimal distances for K aligned blocks until distances vary in arithmetic sequence.
;;    [That's trivial in the case of clear axes.]
;; 4. Given a large N, and a matrix of distances in the 2x2 copies centered in S,
;;    plus the 4 matrix of distances aligned blocks with for K < N,
;;    for each point on the map, add up the number of copies that are reachable
;;    in each aligned and unaligned quadrant.
(def (shift-map v dx dy)
  (with ((vector X Y _) v)
    (def w (make-2d-map X Y))
    (for (x (iota X))
      (for (y (iota Y))
        (xyset! w (modulo (+ x dx) X) (modulo (+ y dy) Y) (xyget v x y))))
    w))
(def (join-map-we w e)
  (with ((vector Xw Yw _) w)
    (with ((vector Xe Ye _) e)
      (assert! (= Yw Ye))
      (def j (make-2d-map (+ Xw Xe) Yw))
      (for (y (iota Yw))
        (for (x (iota Xw)) (xyset! j x y (xyget w x y)))
        (for (x (iota Xe)) (xyset! j (+ x Xw) y (xyget e x y))))
      j)))
(def (join-map-ns n s)
  (with ((vector Xn Yn _) n)
    (with ((vector Xs Ys _) s)
      (assert! (= Xn Xs))
      (def j (make-2d-map Xn (+ Yn Ys)))
      (for (x (iota Xn))
        (for (y (iota Yn)) (xyset! j x y (xyget n x y)))
        (for (y (iota Ys)) (xyset! j x (+ y Yn) (xyget s x y))))
      j)))
(def (d21.2 xym (iterations 26501365))
  (defvalues (xs ys m) (list->values xym))
  (def X (vector-ref m 0))
  (def Y (vector-ref m 1))
  (assert! (= X Y))
  (for (x (iota X))
    (when (> iterations 5000)
      (assert! (eqv? #\. (xygetc m x ys))))
    (assert! (eqv? #\. (xygetc m x 0)))
    (assert! (eqv? #\. (xygetc m x (1- Y)))))
  (for (y (iota Y))
    (when (> iterations 5000)
      (assert! (eqv? #\. (xygetc m xs y))))
    (assert! (eqv? #\. (xygetc m 0 y)))
    (assert! (eqv? #\. (xygetc m (1- X) y))))
  (def ms (shift-map m (- xs) (- ys)))
  ;;(print-2d-map ms)
  (def ms2 (join-map-we ms ms))
  (def ms4 (join-map-ns ms2 ms2))
  (print-2d-map ms4)
  (def 2X (* 2 X))
  (def 2Y (* 2 Y))
  (def dists (make-vector (* 2X 2Y) #f))
  (def (dist-get x y) (vector-ref dists (i<-xy x y 2X 2Y)))
  (def (ensure-dist p d)
    (let* ((i (i<-p p 2X 2Y))
           (dist (vector-ref dists i)))
      (if dist [dist]
          (let (v (d))
            ;;(DBG ed: p v)
            (vector-set! dists i (car v)) v))))
  (def (arcs p) (with-list-builder (c)
                  (for-each-wind (w)
                    (let (q (pos+wind p w))
                      (when (eqv? (pgetc ms4 q) #\.)
                        ;;(DBG arcs: p q)
                        (c [1 . q]))))))
  (A* starts: [[0 [X . Y]]] arcs: arcs bests: ensure-dist)
  ;;(DBG d: dists)
  (def r 0)
  (for (x (iota 2X))
    (for (y (iota 2Y))
      (alet (d (dist-get x y))
        (let ((i (- iterations d)))
          (when (and (<= 0 i) (even? i))
            (let (n (floor-quotient i X))
              (increment! r (half (* (+ n 1) (+ n 2))))))))))
  r)
;; Day 21 Redux
(def d21ex "\
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")
(def (day21 (input (day-input-string 21)))
  (def ex (d21parse d21ex))
  (check (d21.1 ex 6) => 16)
  (check (d21.2 ex 6) => 16)
  (check (d21.2 ex 10) => 50)
#|  (check (d21.2 ex 100) => 6536)
  (check (d21.2 ex 500) => 167004)
  (check (d21.2 ex 1000) => 668697)
  (check (d21.2 ex 5000) => 16733044)|# ;; TODO
  (def m (d21parse input))
  [(d21.1 m) (d21.2 m)]) ;; (3733 ?) ;; 617708918033885 ??


;;; DAY 22 https://adventofcode.com/2223/day/22 -- Simplified Tetris
(def ll1-d22line
  (ll1-list (ll1-begin0 ll1-uint (ll1-char #\,))
            (ll1-begin0 ll1-uint (ll1-char #\,))
            (ll1-begin0 ll1-uint (ll1-char #\~))
            (ll1-begin0 ll1-uint (ll1-char #\,))
            (ll1-begin0 ll1-uint (ll1-char #\,))
            (ll1-begin0 ll1-uint ll1-eolf)))
(def (d22parse t)
  (ll1/string (ll1-repeated ll1-d22line ll1-eof) t))
;; DAY 22 Part 1 --
(def (brick-min-xyz b)
  (with ([x1 y1 z1 x2 y2 z2] b)
    [(min z1 z2) (min x1 x2) (min y1 y2)]))
(def (brick-xyz< b1 b2)
  (lexicographic<? < (brick-min-xyz b1) (brick-min-xyz b2)))
(def (brick-dag m)
  (def N (1+ (length m)))
  (def X (1+ (foldl max 0 (map (match <> ([x1 _ _ x2 _ _] (max x1 x2))) m))))
  (def Y (1+ (foldl max 0 (map (match <> ([_ y1 _ _ y2 _] (max y1 y2))) m))))
  (def Z (1+ (foldl max 0 (map (match <> ([_ _ z1 _ _ z2] (max z1 z2))) m))))
  (def H (make-vector (* X Y) [0 . 0]))
  (def (h xy) (vector-ref H (i<-p xy X Y)))
  (def (h-set! xy a) (vector-set! H (i<-p xy X Y) a))
  (def sup (make-vector N '())) ;; for each brick i, the list of bricks that support it.
  (def pus (make-vector N '())) ;; for each brick i, the list of bricks that it supports
  (def s (map cons (iota (length m) 1) (sort m brick-xyz<)))
  ;;(DBG 22100 N X Y Z H sup pus s)
  (def (bottom p)
    (match p
      ([n x1 y1 z1 x2 y2 z2]
       (cond
        ((not (= x1 x2)) (values z1 (map (lambda (x) [x . y1]) (iota (1+ (abs (- x1 x2))) (min x1 x2)))))
        ((not (= y1 y2)) (values z1 (map (lambda (y) [x1 . y]) (iota (1+ (abs (- y1 y2))) (min y1 y2)))))
        ((not (= z1 z2)) (values (min z1 z2) [[x1 . y1]]))
        (else (values z1 [[x1 . y1]])))))) ;; all equal: single cube
  (def (drop p)
    (defvalues (z l) (bottom p))
    (def z0 0)
    (def supports '(0))
    (for (q l)
      (with ([zq . bq] (h q))
        (cond
         ((> zq z0)
          (set! z0 zq)
          (set! supports (when/list bq [bq])))
         ((= zq z0)
          (when bq (pushnew! bq supports))))))
    (with ([b x y z1 _ _ z2] p)
      (set! (vector-ref sup b) supports)
      (let (dz (- z z0 1))
        (assert! (<= 0 dz))
        (if (= z1 z2)
          (let (nz (- z dz)) (for (q l) (set! (h q) [nz . b])))
          (set! (h [x . y]) [(- (max z1 z2) dz) . b])))))
  (for-each drop s)
  (for (i (iota N))
    (for (j (vector-ref sup i))
      (pushnew! i (vector-ref pus j))))
  (values N sup pus))
(def (d22.1 m)
  (defvalues (N sup pus) (brick-dag m))
  (vector-count (lambda (supported) (andmap (lambda (s) (length>n? (vector-ref sup s) 1)) supported)) pus))
(def (bit->mask n) (arithmetic-shift 1 n))
(def (bits->mask l) (+/list (map bit->mask l)))
(def (mask-fold f s m)
  (let loop ((a s) (o 0) (m m))
    (let (x (first-set-bit m))
      (if (negative? x) a (let (b (+ o x)) (loop (f b a) (1+ b) (arithmetic-shift m (- -1 x))))))))
(def (mask->bits m) (mask-fold cons [] m))
(def (d22.2 m)
  (defvalues (N sup pus) (brick-dag m))
  ;; topologically renumber so a supports b implies a > b, base plate is now (1- N)
  (def o->n (make-vector N 0)) ;; old to new
  (def n->o (make-vector N 0)) ;; new to old
  (def n 0)
  (walk-dag (cut <> 0)
            arrows: (cut vector-ref pus <>)
            synthetic-attribute: (lambda (o _ _)
                                   (set! (vector-ref o->n o) n)
                                   (set! (vector-ref n->o n) o)
                                   (increment! n)))
  (def nsup (make-vector N '()))
  (def npus (make-vector N '()))
  (for (n (iota N))
    (let (o (vector-ref n->o n))
      (set! (vector-ref nsup n) (map (cut vector-ref o->n <>) (vector-ref sup o)))
      (set! (vector-ref npus n) (map (cut vector-ref o->n <>) (vector-ref pus o)))))
  (def disintegrated (make-vector N 0))
  (def weakened (make-vector N 0))
  (for (n (iota N))
    (let loop ((disintegrated* (bit->mask n))
               (weakened* (bits->mask (vector-ref npus n)))
               (cursor n))
      (let (target (1- (integer-length (extract-bit-field cursor 0 weakened*)))) ;; next weakened brick
        (cond
         ((negative? target) ;; done
          (set! (vector-ref disintegrated n) disintegrated*)
          (set! (vector-ref weakened n) weakened*))
         ((andmap (lambda (s) (bit-set? s disintegrated*)) (vector-ref nsup target)) ;; chain reaction!
          (let (dis (bitwise-ior disintegrated* (vector-ref disintegrated target)))
            (loop dis
                  (##bitwise-andc1 dis (bitwise-ior weakened* (vector-ref weakened target)))
                  target)))
         (else
          (loop disintegrated*
                weakened*
                target))))))
  (foldl max 0 (map (lambda (n) (bit-count (vector-ref disintegrated n))) (iota (1- N)))))
;; Day 22 Part 2 --
;; Day 22 Redux
(def d22ex "\
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")
(def (day22 (input (day-input-string 22)))
  (def ex (d22parse d22ex))
  (check (d22.1 ex) => 5)
  (check (d22.2 ex) => 7)
  (def m (d22parse input))
  [(d22.1 m) (d22.2 m)]) ;; 403 1152


;;; DAY 23 https://adventofcode.com/2323/day/23 -- (Build and) Walk a DAG
(def wind-arrows "^v><") ;; arrow for wind blowing TOWARDS given direction
(def (wind->arrow w) (string-ref wind-arrows w))
(def (arrow->wind a) (string-index wind-arrows a))
(def (d23parse m23)
  (def m (parse-2d-map m23))
  (defvalues (X Y V) (vector->values m))
  (def maxY (1- Y))
  (def start-x (let/cc return (for (x (iota X)) (when (eqv? #\. (xygetc m x 0)) (return x)))))
  (def end-x (let/cc return (for (x (iota X)) (when (eqv? #\. (xygetc m x maxY)) (return x)))))
  (def nodes (list->evector []))
  (def h (hash))
  (def (add-node! p)
    (def i (evector-fill-pointer nodes))
    (hash-put! h p i)
    (evector-push! nodes (vector [] [] i p)) ;; remember i and p for debugging reasons only
    i)
  (def start (cons start-x 0))
  (def end (cons end-x maxY))
  (for-each add-node! [start end])
  (def (add-path! start-node end-node len)
    (let (sn (evector-ref nodes start-node)) (push! (cons end-node len) (vector-ref sn 0)))
    (let (en (evector-ref nodes end-node)) (push! (cons start-node len) (vector-ref en 1))))
  (def (orig-winds pos)
    (let ((o 0)
          (a (arrow->wind (pgetc m pos))))
      (for-each-wind (w)
        (when (and (not (eqv? w a))
                   (alet (c (pgetc m (pos+wind pos w)))
                     (not (or (eqv? c #\#)
                              (alet (fw (arrow->wind c))
                                (not (eqv? fw (-wind w))))))))
          (set! o (fxbit-set w o))))
      o))
  (def (dest-winds pos)
    (let ((o 0)
          (a (arrow->wind (pgetc m pos))))
      (for-each-wind (w)
        (when (and (or (not a) (eqv? w a))
                   (alet (c (pgetc m (pos+wind pos w)))
                     (or (eqv? c #\.)
                         (alet (tw (arrow->wind c))
                           (not (eqv? tw (-wind w)))))))
          (set! o (fxbit-set w o))))
      o))
  ;; Search paths: from current point to next point with more than one choice.
  (def (search-paths start-node pos wind len)
    (if-let (end-node (hash-get h pos))
      ;; found a path to a known node, add it and stop.
      (add-path! start-node end-node len)
      ;; new position, examine its destinations
      (let* ((dests (dest-winds pos))
             (allowed-dests (fxbit-clear (-wind wind) dests)))
        ;; stop (without successful result) at dead ends
        (unless (fxzero? allowed-dests)
          (let (origs (orig-winds pos))
            (if (fx< 2 (bit-count (fxior origs dests))) ;; found new node!
                (let (node (add-node! pos))
                  (add-path! start-node node len)
                  (for-each-wind (w dests) (search-paths node (pos+wind pos w) w 1)))
                (let (w (fxfirst-set-bit allowed-dests))
                  (search-paths start-node (pos+wind pos w) w (1+ len)))))))))
  ;; From the start node and position, head Southward with 1 step.
  (search-paths 0 (pos+wind start 1) 1 1)
  (evector->vector nodes))
;; DAY 23 Part 1 -- The graph better be a dag
(def (d23.1 nodes)
  (walk-dag (cut <> 0)
            arrows: (lambda (n) (vector-ref (vector-ref nodes n) 0))
            arrow-target: car
            synthetic-attribute:
            (lambda (_ arrs lens)
              (foldl max 0 (map (lambda (a l) (+ (cdr a) l)) arrs lens)))))
;; Day 23 Part 2 --
;; Day 23 Redux
(def d23ex "\
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#")
(def (day23 (input (day-input-string 23)))
  (def ex (d23parse d23ex))
  (check (d23.1 ex) => 94)
  (def m (d23parse input))
  [(d23.1 m)]) ;; 2030


;;; DAY 24 https://adventofcode.com/2424/day/24 -- Inverting 2x2 matrices
(def ll1-d24hailstone
  (ll1-list (ll1-begin0 ll1-uint (ll1-string ",") (ll1-char* " "))
            (ll1-begin0 ll1-uint (ll1-string ",") (ll1-char* " "))
            (ll1-begin0 ll1-uint (ll1-string " @") (ll1-char* " "))
            (ll1-begin0 ll1-sint (ll1-string ",") (ll1-char* " "))
            (ll1-begin0 ll1-sint (ll1-string ",") (ll1-char* " "))
            (ll1-begin0 ll1-sint ll1-eolf)))
(def (d24parse hail)
  (list->vector (ll1/string (ll1-repeated ll1-d24hailstone ll1-eof) hail)))
;; DAY 24 Part 1
(def (hail-intersect? hail1 hail2 minXY maxXY)
  (with ([x10 y10 _ vx1 vy1 _] hail1)
    (with ([x20 y20 _ vx2 vy2 _] hail2)
      ;; intersection: x = x10 + vx1*t1 = x20 + vx2*t2     (vx1 -vx2) (t1) = (x20-x10)
      ;;               y = y10 + vy1*t1 = y20 + vy2*t2     (vy1 -vy2) (t2)   (y20-y10)
      ;; d = -vx1*vy2+vy1*vx2
      ;; t1 = (-vy2*(x20-x10) + vx2*(y20-y10))/d
      ;; t2 = (-vy1*(x20-x10) + vx1*(y20-y10))/d
      (let/cc return
        (defrule (fail) (return #f))
        (let* ((d (- (* vy1 vx2) (* vx1 vy2)))
               (_d (when (zero? d) (fail)))
               (t1 (/ (+ (* vy2 (- x10 x20)) (* vx2 (- y20 y10))) d))
               (_t1 (when (negative? t1) (fail)))
               (t2 (/ (+ (* vy1 (- x10 x20)) (* vx1 (- y20 y10))) d))
               (_t2 (when (negative? t2) (fail)))
               (x (+ x10 (* vx1 t1)))
               (_x (unless (<= minXY x maxXY) (fail)))
               (y (+ y10 (* vy1 t1)))
               (_y (unless (<= minXY y maxXY) (fail)))
               #;(_ (DBG hi: hail1 hail2 d t1 t2 x y)))
          #t)))))
(def (d24.1 hail (minXY 200000000000000) (maxXY 400000000000000))
  (def N (vector-length hail))
  (def c 0)
  (for (i (in-range N))
    (for (j (in-range (1+ i) N))
      (when (hail-intersect? (vector-ref hail i) (vector-ref hail j) minXY maxXY)
        (increment! c))))
  c)
;; Day 24 Part 2 --
;; Day 24 Redux
(def d24ex "\
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3")
(def (day24 (input (day-input-string 24)))
  (def ex (d24parse d24ex))
  (check (d24.1 ex 7 27) => 2)
  (def m (d24parse input))
  [(d24.1 m)]) ;; (24192)

(def (main . _)
  (nest
   (time)
   (writeln)
   #;(let (r (PeekableStringReader (open-buffered-string-reader d24ex)))
       (try (ll1-d24rule r) (catch (e) (displayln e) ((ll1-char* char?) r))))
   (day24)))

(main)
