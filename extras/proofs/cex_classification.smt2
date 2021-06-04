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

;; Counterexample x = -1, f(l) = -2?
;; Sat-checking strategy: does not work

;;(declare-const x Int)
;;(declare-const l List)

;;(assert (is_sorted (cons x l)))
;;(assert (= x (- 1)))
;;(assert (= (min l) (- 2)))

;; Using quantifiers: checking
;; ∃ x : int , l : List, (x = -1) ∧ min(l) = -2 ∧ is_sorted(cons(x,l))
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