(set-logic ALL)
(set-option :produce-models true)

(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))

 ;; i1 ≥ 0 => max ((max (i1 + n) 0) + n3) 0 = join1 n i1 (max n3 0)

(declare-fun join1 (Int Int Int) Int)

(assert
    (forall ((i1 Int) (n Int) (n3 Int))
    (=> (>= i1 0) (= (max (+ (max (+ i1 n) 0) n3) 0) (join1 n i1 (max n3 0))))))

(check-sat)
;; This should be unsat, but the solver answers "unknown"

