(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join ((x0 Int) (x1 Int)) Int
 ((Start Int (Ic x0 x1 (- Start) (+ Start Start) (max Start Start))) (Ic Int ((Constant Int)))))
(declare-var i Int)
(declare-var i0 Int)
(constraint (or (not (>= i0 0)) (= i0 (join 0 i0))))
(constraint (or (not (>= i0 0)) (= (max (+ i0 i) 0) (join (max (+ 0 i) 0) i0))))
(check-synth)