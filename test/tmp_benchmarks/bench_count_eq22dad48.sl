(set-logic LIA)
(synth-fun f0 ((x13 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x13 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i146 Int)
(declare-var i Int)
(declare-var a Int)
(constraint (or (not (and (or (not (= i a)) (= i146 0)) (= i a))) (= i146 (f0 0))))
(check-synth)
