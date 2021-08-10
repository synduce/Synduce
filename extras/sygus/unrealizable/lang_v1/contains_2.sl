(set-logic LIA)
(synth-fun xi_1 ((x5 Int)) Int
 ((Start Int (Ic x5 (- Start) (+ Start Start) (ite Ipred Start Start))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Start Start) (> Start Start) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var x Int)
(declare-var i Int)
(declare-var i1 Int)
(declare-var i2 Int)
(constraint
 (or (not (and (and (and (>= i1 0) (<= i1 1)) (and (>= i2 0) (<= i2 1))) (< x i)))
  (= (ite (= i x) 1 (ite (= i1 1) 1 (ite (= i2 1) 1 0))) (xi_1 i1))))
(check-synth)
