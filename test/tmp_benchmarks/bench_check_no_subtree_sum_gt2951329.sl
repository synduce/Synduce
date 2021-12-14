(set-logic LIA)
(synth-fun xi_2 ((x7 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x7 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i4 Int)
(declare-var i3 Int)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint (or (not (not (> i0 2))) (= (+ (+ i3 i4) i0) (xi_2 i1))))
(check-synth)
