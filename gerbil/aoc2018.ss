;; Solutions to https://AdventOfCode.com/2018

(import
  :std/iter :std/misc/list :std/misc/repr :std/srfi/1 :std/sugar
  :clan/utils/assert :clan/utils/base :clan/utils/basic-parsers
  :clan/utils/generator :clan/utils/hash :clan/utils/number)


;;; DAY 1 https://adventofcode.com/2018/day/1

(defsyntax (this-source-directory stx)
  (syntax-case stx ()
    ((_)
     (let (path (gx#core-resolve-path "" (stx-source stx)))
       (with-syntax ((txt path))
         #'(quote txt))))))

(def day1-input-file (path-expand "aoc2018-1.input" (this-source-directory)))

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

(def (day1-input (file day1-input-file))
  (nest
   (generating<-for-each) (λ (yield))
   (call-with-input-file file) (λ (port))
   (for-each-port-signed-integer! port yield)))

(def day1-answer1 (reduce + 0 (list<-generating (day1-input)))) ;; 445

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

(def day1-answer2
  (generating-find-first-duplicate
   (generating-sums
    (generating-indefinitely day1-input)))) ;; 219


;;; DAY 2 https://adventofcode.com/2018/day/2

(def day2-input-file (path-expand "aoc2018-2.input" (this-source-directory)))

(def (day2-input (file day2-input-file)) (read-file-lines file))

(def (hash-increment! table key (increment 1))
  (let ((previous (hash-ref table key 0)))
    (hash-put! table key (+ previous increment))))

(def (letter-counts id)
  (let ((counts (hash)))
    (string-for-each (cut hash-increment! counts <>) id)
    counts))

(def (invert-letter-counts id)
  (invert-hash (letter-counts id)))

(def (day2-checksum input)
  (let* ((invert-counts (map invert-letter-counts input))
         (containing-exactly-2-of-some-letter (length (filter (cut hash-key? <> 2) invert-counts)))
         (containing-exactly-3-of-some-letter (length (filter (cut hash-key? <> 3) invert-counts))))
    (* containing-exactly-2-of-some-letter containing-exactly-3-of-some-letter)))

(def day2-answer1 (day2-checksum (day2-input))) ;; 5456

(def (least-string-difference-index x y)
  (let ((lx (string-length x))
        (ly (string-length y)))
    (letrec ((r (λ (i)
                  (if (<= lx i) (if (<= ly i) #f i)
                      (if (<= ly i) i
                          (if (eqv? (string-ref x i) (string-ref y i)) (r (+ i 1)) i))))))
      (r 0))))

(def (string-pair-single-different-index x y)
  (nest
   (let ((lx (string-length x))
         (ly (string-length y))))
   (and (= lx ly))
   (let ((i (least-string-difference-index x y))))
   (and i)
   (let ((j (+ i 1))))
   (and (string= x y j lx j lx))
   i))

(def (string-pair-without-single-difference x y)
  (let ((i (string-pair-single-different-index x y)))
    (and i (string-append (substring x 0 i) (substring x (+ i 1) (string-length x))))))

(def (list-for-each-pair! l1 l2 f)
  (for-each! l1 (λ (x1) (for-each! l2 (λ (x2) (f x1 x2))))))

(def (list-for-each-couple! l f)
  (letrec ((r1 (λ (l)
                 (unless (null? l)
                   (r2 (car l) (cdr l)))))
           (r2 (λ (head tail)
                 (unless (null? tail)
                   (for-each! tail (λ (x) (f head x)))
                   (r1 tail)))))
    (r1 l)))

(def day2-answer2
  (nest
   (let/cc k)
   (list-for-each-couple! (day2-input)) (λ (x y))
   (when-let (r (string-pair-without-single-difference x y)))
   (k r))) ;; "megsdlpulxvinkatfoyzxcbvq"

