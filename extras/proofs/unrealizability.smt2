;; z3 returns unsat and a proof for this problem.
;; At first sight, we cannot extract counterexamples from this proof.
;; cvc5 returns unknown for this problem.
(set-logic UFLIA)
(set-option :produce-proofs true)

(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))

(declare-fun join (Int Int) Int)

(assert
    (forall ((i Int) (i0 Int))
        (and (or (not (>= i0 0)) (= i0 (join 0 i0)))
         (or (not (>= i0 0)) (= (max (+ i0 i) 0) (join (max (+ 0 i) 0) i0))))))
(check-sat)
(get-proof)
