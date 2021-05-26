(set-logic ALL)
(set-option :produce-models true)

(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))

 ;; i1 ≥ 0 => max ((max (i1 + n) 0) + n3) 0 = join1 n i1 (max n3 0)


(declare-const i1 Int)
(declare-const n Int)
(declare-const n3 Int)
(declare-const i1_ Int)
(declare-const n_ Int)
(declare-const n3_ Int)

(assert (>= i1 0))
(assert (>= i1_ 0))

(assert (not (= (max (+ (max (+ i1 n) 0) n3) 0) (max (+ (max (+ i1_ n_) 0) n3_) 0))))
(assert (= n n_))
(assert (= i1 i1_))
(assert (= (max n3 0) (max n3_ 0)))


(check-sat)
;; This should sat
(get-model)

