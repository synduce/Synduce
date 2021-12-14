(set-logic LIA)
(synth-fun f0 ((x12 Int) (x13 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x12 x13 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i136 Int)
(declare-var i135 Int)
(declare-var p Int)
(constraint (or (not (= i135 i136)) (= (+ (+ 1 i135) i136) (f0 p i135))))
(check-synth)
