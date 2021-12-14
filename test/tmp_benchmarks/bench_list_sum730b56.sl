(set-logic LIA)
(synth-fun s0 () Int)
(synth-fun join ((x6 Int) (x7 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x6 x7 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i0 Int)
(constraint (= i0 (join s0 i0)))
(check-synth)
