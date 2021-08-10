(set-logic LIA)
(synth-fun f0 ((x2 Int)) Int
 ((Start Int (Ic x2 (- Start) (+ Start Start))) (Ic Int ((Constant Int)))))
(declare-var a Int)
(declare-var i Int)
(declare-var i0 Int)
(constraint (or (not (= i a)) (= (+ 1 i0) (f0 i))))
(check-synth)
