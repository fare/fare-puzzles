;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(defsystem "fare-puzzles"
  :description "Various puzzles"
  :long-description "Solutions to various puzzles"
  :author "Francois-Rene Rideau"
  :license "MIT"
  :class :package-inferred-system)

(defsystem "fare-puzzles/all"
  :depends-on
  ("fare-puzzles/codeeval/codeeval-39"
   "fare-puzzles/euler/euler4"
   "fare-puzzles/euler/euler37"
   ;; NB: ITA and misc puzzles haven't been properly packaged yet.
   "fare-puzzles/words/concordance"
   "fare-puzzles/words/with-friends"))
