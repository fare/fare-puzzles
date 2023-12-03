(import
  :std/error
  (only-in :std/parser/base parse-error?)
  :std/sugar
  :std/test
  :std/text/char-set
  "./ll1-parser")

(export ll1-parser-test)


(defrule (check-parse parser string result)
  (begin
    (check-equal? (ll1/string parser string) result)
    (check-equal? (call-with-input-string string (cut ll1/port parser <>)) result)))
(defrule (check-parse-error parser string)
  (begin
    (check-exception (ll1/string parser string) parse-error?)
    (check-exception (call-with-input-string string (cut ll1/port parser <>)) parse-error?)))

(def ll1-parser-test
  (test-suite "test suite for std/text/ll1-parser"
    (test-case "empty"
      (check-parse ll1-empty "" (void))
      (check-parse (ll1-result 42) "" 42)
      (check-parse (ll1-pure 42) "" 42)
      (check-parse-error ll1-empty "foo")
      (check-parse-error (ll1-result 42) "foo")
      (check-parse-error (ll1-pure 42) "foo"))
    (test-case "1"
      (check-parse ll1-uint "1" 1)
      (check-parse ll1-uint "010" 10) ;; ain't no octal
      (check-parse (cut ll1-uint <> 8) "10" 8) ;; octal this time.
      (check-parse-error ll1-uint " 1") ;; no space allowed in front unless you ask
      (check-parse-error ll1-uint "1 no junk allowed"))
    (test-case "char"
      (check-parse (ll1-char "fo") "f" #\f)
      (check-parse (ll1-char "fo") "o" #\o)
      (check-parse (ll1-char char-ascii-alphabetic?) "a" #\a))
    ))
