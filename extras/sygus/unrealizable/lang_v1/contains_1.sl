(set-logic LIA)
(synth-fun xi_1 ((x1 Int) (x2 Int)) Int
 ((Start Int (Ic x1 x2 (- Start) (+ Start Start) (ite Ipred Start Start))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Start Start) (> Start Start) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var x Int)
(declare-var p Int)
(declare-var i Int)
(constraint (or (not (and (>= i 0) (<= i 1))) (= (ite (= p x) 1 i) (xi_1 x p))))
(check-synth)
