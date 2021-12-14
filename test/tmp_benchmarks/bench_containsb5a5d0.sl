(set-logic LIA)
(synth-fun xi_1 ((x31 Int) (x32 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x31 x32 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i24 Int)
(declare-var p Int)
(declare-var x Int)
(constraint
 (or (not (and (and (>= i24 0) (<= i24 1)) (= x (ite (= i24 0) x p))))
  (= (ite (= p x) 1 i24) (xi_1 x p))))
(check-synth)
