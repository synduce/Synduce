(set-logic LIA)
(synth-fun xi_1 ((x2 Int) (x3 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x2 x3 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i Int)
(declare-var p Int)
(declare-var x Int)
(constraint (or (not (>= i 0)) (= (ite (= p x) 1 (ite (= i 0) 0 (+ 1 i))) (xi_1 x p))))
(check-synth)
