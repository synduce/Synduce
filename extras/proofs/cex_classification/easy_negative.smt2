(set-logic ALL)
;; Options --quant-ind [--quant-cf --full-saturate-quant]
(set-option :quant-ind true)

(declare-datatype List ((base (a Int) (b Int)) (cons (hd Int) (tl List))))

(define-fun head ((x List)) Int
    (match x
        (
            ((base a b) a)
            ((cons hd tl) hd)
        ))
)

;; Axioms

;; Definition of is_sorted
(define-fun-rec is_sorted ((x List)) Bool
    (match x
        (
            ((base a b) (<= a b))
            ((cons x l) (and (is_sorted l) (<= x (head l))))
        )
    )
)

;; Definition of min (for lists)
(define-fun-rec min ((x List)) Int
    (match x
        (
            ((base a b) (ite (<= a b) a b))
            ((cons a l) (ite (<= (min l) a) (min l) a))
        )
    )
)

;; ∃ x : int, l : List. x = -1 ∧ min(l) = -2 ∧ is_sorted(cons(x,l))
(assert
        (exists
            ((x Int) (l List))
            (and
                (= x (- 1))
                (= (min l) (- 2))
                (is_sorted (cons x l))
            )
        )
)

(check-sat)
;; Returns unsat: this is a "negative" counterexample,