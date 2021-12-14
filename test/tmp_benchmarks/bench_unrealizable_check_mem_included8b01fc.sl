(set-logic LIA)
(synth-fun xi_12 ((x45 Bool)) Bool ((Ipred Bool))
 ((Ipred Bool (x45 (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b3 Bool)
(declare-var b2 Bool)
(declare-var i0 Int)
(declare-var i Int)
(declare-var lo Int)
(declare-var hi Int)
(constraint
 (or (not (and (and (or (not (and (not (> i lo)) (> i0 hi))) (> i0 i)) (> i0 hi)) (not (> i lo))))
  (= (or (and (< lo i) (< i0 hi)) (or b2 b3)) (xi_12 b3))))
(check-synth)
