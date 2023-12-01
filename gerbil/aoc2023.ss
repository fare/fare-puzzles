;; Solutions to https://AdventOfCode.com/2023

(import
  :gerbil/gambit
  :std/assert
  :std/debug/DBG
  :std/format
  :std/iter
  :std/misc/bytes
  :std/misc/list
  :std/misc/number
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
  :clan/matrix)

;;; General purpose utilities

(def (day-input-file n) (path-expand (format "data/aoc2023-~d.input" n) (this-source-directory)))
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
    (check (x2 '("two1nine"
                 "eightwothree"
                 "abcone2threexyz"
                 "xtwone3four"
                 "4nineeightseven2"
                 "zoneight234"
                 "7pqrstsixteen")) => 281)))
(check-day1 day1.1 day1.2)
(check-day1 day1.1* day1.2*)

(def (day1 (input (read-file-lines (day-input-file 1))))
  (check (day1.1 input) => (day1.1* input))
  (check (day1.2 input) => (day1.2* input))
  [(day1.1* input) (day1.2* input)])

