(set-logic ALL)
;; Options --quant-ind [--quant-cf --full-saturate-quant]
(set-option :quant-ind true)
(set-option :produce-models true)

;; Axioms

;; Definition of min (for lists)
(define-fun min ((x Int) (y Int)) Int
    (ite (>= x y) y x)
)

;; Is there a list of length 3 that satisfies the constraints?
(declare-const x Int)
(declare-const a1 Int)
(declare-const a2 Int)
(declare-const a3 Int)

(assert (= x (- 1)))
(assert
    (=
        (min a1 (min a2 a3))
        1
    )
)

(assert (and (<= x a1) (<= a1 a2) (<= a2 a3)))


(check-sat)
;; Returns sat: this is a "positive" counterexample,
(get-model)