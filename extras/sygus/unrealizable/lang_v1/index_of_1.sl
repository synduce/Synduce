(set-logic LIA)
(synth-fun xi_1 ((x4 Int) (x5 Int)) Int
 ((Start Int (Ic x4 x5 (- Start) (+ Start Start) (ite Ipred Start Start))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Start Start) (> Start Start) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var x Int)
(declare-var i Int)
(declare-var i1 Int)
(constraint
 (or (not (and (>= i1 0) (>= i x))) (= (ite (= i x) 1 (ite (= i1 0) 0 (+ 1 i1))) (xi_1 x i))))
(check-synth)
