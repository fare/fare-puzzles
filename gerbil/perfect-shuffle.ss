;; Let’s say you have a perfectly sorted deck of cards. You can also perform a perfect shuffle,
;; i.e., you can split the cards into two stacks of 26 and when you shuffle, the two stacks
;; alternate perfectly, with the first card of the undivided deck still on top.
;; If you keep repeating the perfect shuffle, is it possible to get back to the initial state?
;; If so, what number of perfect shuffles would it take?

(import :std/srfi/1 :std/iter :std/misc/repr :clan/utils/base)

(def (alternate first second)
  (cond
   ((null? first) second)
   ((null? second) first)
   (else (cons* (car first) (car second) (alternate (cdr first) (cdr second))))))

(def (split-cards cards)
  (def l (length cards))
  (def h (quotient l 2))
  (split-at cards h))

(def (shuffle cards) (call/values (λ () (split-cards cards)) alternate))

(def (deck (n 52)) (iota n 0))

(def (apply-permutation p l)
  (def v (list->vector l))
  (for/collect (i p) (vector-ref v i)))

(def compose-permutations apply-permutation)

(def (iterate-permutation n p)
  (def q (quotient n 2))
  (def r (modulo n 2))
  (if (> q 0)
    (let ((dqp (iterate-permutation q (compose-permutations p p))))
      (if (zero? r) dqp (compose-permutations dqp p)))
    (if (zero? r) (iota (length p) 0) p)))

;; (pr (iterate-permutation 8 (shuffle (deck)))) <--- 8
;; (pr (iterate-permutation 4 (shuffle (deck))))

;; Determine a permutation's order as the LCM of its element's cycle's length.
(def (permutation-cycle-order pv i)
  (def (permute j) (vector-ref pv j))
  (def (next k l) (curr (permute k) (+ 1 l)))
  (def (curr k l) (if (eqv? i k) l (next k l)))
  (next i 0))

(def (permutation-cycle pv i)
  (def order (permutation-cycle-order pv i))
  (def cv (make-vector order 0))
  (for (j (iota order))
    (vector-set! cv j i)
    (set! i (vector-ref pv i)))
  cv)

(def (permutation-order p)
  (def l (length p))
  (def pv (list->vector p))
  (def seen? (make-vector l #f))
  (def order 1)
  (def (permute i) (vector-ref pv i))
  (def (next-unseen i) (cond ((>= i l) #f) ((vector-ref seen? i) (next-unseen (+ i 1))) (else i)))
  (def (find-order! i)
    (def n (next-unseen i))
    (when n
      (consider-cycle! n)
      (find-order! (+ n 1))))
  (def (consider-cycle! n)
    (def cv (permutation-cycle pv n))
    (def co (vector-length cv))
    (set! order (lcm order co))
    (for (i (iota co)) (vector-set! seen? (vector-ref cv i) #t)))
  (find-order! 0)
  order)

(pr (for/collect (n (iota 54 1)) (permutation-order (shuffle (deck n)))))
