(set-logic LIA)
(synth-fun f0 ((x4 Int) (x5 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x4 x5 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i0 Int)
(declare-var i Int)
(declare-var p Int)
(constraint (= (+ (+ 1 i) i0) (f0 p i)))
(check-synth)
