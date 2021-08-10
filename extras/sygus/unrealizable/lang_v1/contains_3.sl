(set-logic LIA)
(synth-fun xi_2 ((x1 Int) (x2 Int) (x3 Int)) Int
 ((Start Int (Ic x1 x2 x3 (- Start) (+ Start Start) (ite Ipred Start Start))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Start Start) (> Start Start) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var x Int)
(declare-var p Int)
(declare-var i Int)
(declare-var i0 Int)
(constraint
 (or (not (and (and (>= i 0) (<= i 1)) (and (>= i0 0) (<= i0 1))))
  (= (ite (= p x) 1 (ite (= i 1) 1 (ite (= i0 1) 1 0))) (xi_2 x p i))))
(check-synth)
