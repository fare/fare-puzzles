#!/usr/bin/env gxi
;; Parse and re-print data copy/pasted from BoA banking statements using evince

(export #t)

(import
  :std/format :std/getopt :std/iter :std/misc/shuffle
  :std/srfi/1 :std/srfi/13 :std/srfi/43
  :clan/assert :clan/base :clan/basic-parsers :clan/cli :clan/debug
  :clan/multicall :clan/path :clan/random :clan/source
  :clan/poo/cli)

(def (index<-letter l)
  (string-index "ABCD" l))
(def (letter<-index i)
  (string-ref "ABCD" i))

(def (expect-question port)
  (def qn (expect-natural port))
  ((expect-literal-string ". ") port)
  (def qs (expect-line port))
  (def qa (for/collect (l "ABCD")
            ((expect-char l) port)
            ((expect-literal-string ". ") port)
            (expect-line port)))
  [qn qs . qa])
(def (expect-answer port)
  (def an (expect-natural port))
  ((expect-literal-string ". ") port)
  (def al ((expect-one-of index<-letter) port))
  (expect-eol port)
  [an al])

(def (expect-questions port)
  (concatenate
   (for/collect (chapter (in-range 2 5))
     (def questions ((expect-begin
                      (expect-literal-string (format "CHAPTER ~d REVIEW QUESTIONS~%" chapter))
                      (expect-repeated expect-question expect-eol)) port))
     (def answer-key ((expect-begin
                       (expect-literal-string (format "CHAPTER ~d ANSWER KEY~%" chapter))
                       (expect-repeated expect-answer expect-eol)) port))
     (map (lambda (question answer-key)
            (match [question answer-key]
              ([[qn qs a b c d] [an al]]
               (assert-equal! qn an)
               [chapter qn qs a b c d al])))
          questions answer-key))))

(def (parse-questions)
  (def data (subpath (this-source-directory) "data/questions.text"))
  (parse-file data expect-questions))

(def ask-question
  (match <>
    ([ch n qs a b c d al]
     (def aa (vector a b c d))
     (def si (vector-shuffle #(0 1 2 3)))
     (printf "~d.~d. ~a\n" ch n qs)
     (vector-for-each (lambda (i j) (displayln (letter<-index i) ". " (vector-ref aa j))) si)
     (def choice (ignore-errors (parse-string (string-upcase (read-line)) (expect-one-of index<-letter))))
     (def solution (letter<-index (vector-index (lambda (x) (eqv? x (index<-letter al))) si)))
     (if (eqv? choice solution)
       (displayln "CORRECT!")
       (displayln "WRONG! The correct answer was: " solution
                  " (" al " in the manual)"))
     (newline))))

(define-entry-point (quizz shuffle?: (shuffle? #f))
  (help: "Quizz"
   getopt: (make-options [(flag 'shuffle? "--shuffle" help: "shuffle the order of the questions")]))
  (randomize!)
  (def questions (parse-questions))
  (when shuffle?
    (set! questions (shuffle questions)))
  (for (q questions)
    (ask-question q)))

(def (main . args) (apply call-entry-point "quizz" args))
