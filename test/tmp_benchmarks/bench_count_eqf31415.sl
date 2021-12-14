(set-logic LIA)
(synth-fun f0 ((x12 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x12 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i12 Int)
(declare-var i Int)
(declare-var a Int)
(constraint (or (not (and (or (not (= i a)) (= i12 0)) (= i a))) (= (+ 1 i12) (f0 i))))
(check-synth)
