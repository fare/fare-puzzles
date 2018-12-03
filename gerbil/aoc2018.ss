;; Solutions to https://AdventOfCode.com/2018

(import
  :std/iter :std/misc/list :std/misc/repr :std/srfi/1 :std/sugar
  :clan/utils/assert :clan/utils/base :clan/utils/basic-parsers :clan/utils/generator)
;;(extern (this-source-file this-source-file))

;;; DAY 1, PART 1 https://adventofcode.com/2018/day/1

(defsyntax (this-source-directory stx)
  (syntax-case stx ()
    ((_)
     (let (path (gx#core-resolve-path "" (stx-source stx)))
       (with-syntax ((txt path))
         #'(quote txt))))))

(def input-file (path-expand "aoc2018-1.input" (this-source-directory)))

(def (expect-signed-integer port (base 10))
  (let ((char (peek-char port)))
    (cond
     ((eqv? char #\+)
      (read-char port)
      (expect-natural port base))
     ((eqv? char #\-)
      (read-char port)
      (- (expect-natural port base)))
     ((char-digit char)
      (expect-natural port base))
     (else
      (parse-error! 'expect-signed-integer "Neither a sign nor a digit in requested base"
                    char base port)))))

(def (for-each-port-signed-integer! port fun)
  (until (port-eof? port)
    (fun (expect-signed-integer port))
    (expect-and-skip-any-whitespace port)))

(def (input (file input-file))
  (nest
   (generating<-for-each) (λ (yield))
   (call-with-input-file file) (λ (port))
   (for-each-port-signed-integer! port yield)))

(def (list<-input) (list<-generating (input)))

(def day1-part1-answer (reduce + 0 (list<-input))) ;; 445

;;; DAY 1, part 2

(def (generating-decons on-cons on-eof g)
  (let/cc k (on-cons (g (λ () (k (on-eof)))))))

;;(def (generating-cons x g) (λ ((on-eof eof!)) (let/cc k (k x) (g on-eof))) ;; doesn't work. TODO: debug
(def (generating-cons x g) (generating-concat (generating-singleton x) g))

;; This function fails somehow (!)
(def (generating-reduce-0 f seed g)
  (letrec ((r (λ (z)
                (prn ['gr z]) ;; prints correct z
                (λ ((on-eof eof!))
                  (generating-decons
                   (λ (x)
                     (prn ['gr1 z x]) ;; BUG: prints 0 for z (!) somehow the function closes over the initial seed instead of z.
                     (let ((y (f z x)))
                       (prn ['gr2 x z y])
                       ((generating-cons y (r y)) on-eof)))
                   on-eof g)))))
    (r seed)))

(def (generating-reduce f seed g)
  (nest
   (generating<-for-each) (λ (yield))
   (generating-for-each! g)
   (λ (x) (set! seed (f seed x)) (yield seed))))

(def (generating-sums g) (generating-reduce + 0 g))

(def (generating-find-first-duplicate g)
  (nest
   (let ((previous (hash)))) (let/cc k)
   (generating-for-each! g) (λ (x))
   (begin ())
   (if (hash-key? previous x) (k x))
   (hash-put! previous x #t)))

(def (generating-indefinitely gg)
  (generating<-for-each (λ (yield) (while #t (generating-for-each! (gg) yield)))))

(def day1-part2-answer
  (generating-find-first-duplicate
   (generating-sums
    (generating-indefinitely input)))) ;; 219
