(set-logic LIA)
(synth-fun xi_2 ((x6 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x6 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i2 Int)
(declare-var i1 Int)
(declare-var i Int)
(declare-var x Int)
(constraint (or (not (and (and (>= i1 0) (>= i2 0)) (not (< i x)))) (= (+ i1 i2) (xi_2 i1))))
(check-synth)
