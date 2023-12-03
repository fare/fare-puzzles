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
  :std/misc/list
  :std/misc/number
  :std/misc/path
  :std/misc/ports
  :std/misc/repr
  :std/misc/string
  :std/sort
  :std/source
  :std/srfi/1
  :std/srfi/13
  :std/srfi/43
  :std/sugar
  :std/test
  :std/text/basic-parsers
  :std/text/basic-printers
  :std/text/char-set
  :clan/assert
  :clan/base
  :clan/matrix
  "./ll1-parser" ;; to be moved to std/text/ll1-parser
  )

;;; General purpose utilities

(def (day-input-file n) (subpath (this-source-directory) (format "data/aoc2023-~d.input" n)))
(def u8vector-ref-set! u8vector-set!)
(def vector-ref-set! vector-set!)

(def (+/list l) (foldl + 0 l))
(def digit-names #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

;;; DAY 1 https://adventofcode.com/2023/day/1

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
(def (day1.0 frob input) (+/list (map (compose calibration-value frob) input)))
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

(def (day1.0* first last input) (+/list (map (cut calibration-value* first last <>) input)))
(def (day1.1* input) (day1.0* first-digit last-digit input))
(def (day1.2* input) (day1.0* first-digit* last-digit* input))

(defrule (check-day1 x1 x2)
  (begin
    (check (x1 '("1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet")) => 142)
    (check (x2 '("two1nine" "eightwothree" "abcone2threexyz" "xtwone3four"
                 "4nineeightseven2" "zoneight234" "7pqrstsixteen")) => 281)))

(def (day1 (input (read-file-lines (day-input-file 1))))
  (check-day1 day1.1 day1.2)
  (check-day1 day1.1* day1.2*)
  (check (day1.1 input) => (day1.1* input))
  (check (day1.2 input) => (day1.2* input))
  [(day1.1* input) (day1.2* input)])

;;; DAY 2 https://adventofcode.com/2023/day/2

;; Parser for games
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

(def (possible-drawing? content drawing)
  (andmap (match <> ([color . n] (<= n (assgetq color content)))) drawing))
(def (possible-game? content game)
  (andmap (cut possible-drawing? content <>) (cdr game)))
(def (sum-possible-games content games)
  (+/list (map car (filter (cut possible-game? content <>) games))))
(def (day2.1 games)
  (sum-possible-games '((red . 12) (green . 13) (blue . 14)) games))

(def (game-minima game)
  (def h (hash (red 0) (green 0) (blue 0)))
  (for-each (lambda (drawing)
              (for-each (match <> ([c . n] (unless (<= n (hash-get h c)) (hash-put! h c n))))
                        drawing))
            (cdr game))
  h)
(def (game-power game)
  (foldl * 1 (hash-values (game-minima game))))
(def (day2.2 games)
  (+/list (map game-power games)))

(def day2-example
  (ll1/string ll1-games
              "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

(def (day2 (input (ll1/file ll1-games (day-input-file 2))))
  (check (day2.1 day2-example) => 8)
  (check (day2.2 day2-example) => 2286)
  (def r [(day2.1 input) (day2.2 input)])
  (check r => [2505 70265])
  r)

;;; DAY 3 https://adventofcode.com/2023/day/3

;; Parser for games

(def day3-example
"\
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
(def (day3 (input (read-file-string (day-input-file 3))))
  (check (day3.1 day3-example) => 4361)
  (check (day3.2 day3-example) => 467835)
  [(day3.1 input) (day3.2 input)])
