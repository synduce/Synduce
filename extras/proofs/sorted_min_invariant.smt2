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
(declare-fun is_sorted (List) Bool)
;; "Declarative" definition can be directly extracted from functional definition
(assert (forall ((x Int) (y Int)) (= (<= x y) (is_sorted (base x y)))))
(assert (forall ((x Int) (l List)) (= (is_sorted (cons x l)) (and (is_sorted l) (<= x (head l))))))

;; Definition of min (for lists)
(declare-fun min (List) Int)
(assert (forall ((x Int) (y Int)) (= (min (base x y)) (ite (<= x y) x y))))
(assert (forall ((x Int) (l List)) (= (min (cons x l)) (ite (<= x (min l)) x (min l)))))

;; Conjecture:
;; if (Cons x l) is sorted then x < min(l)
(assert
    (not
        (forall ((x Int) (l List))
            (=> (is_sorted (cons x l)) (<= x (min l)))
        )
    )
)

(check-sat)
;; Should be unsat, i.e. conjecture holds