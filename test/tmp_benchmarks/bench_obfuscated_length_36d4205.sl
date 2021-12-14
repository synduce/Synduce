(set-logic LIA)
(synth-fun join ((x2 Int) (x3 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x2 x3 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i0 Int)
(declare-var p Int)
(constraint (or (not (>= (+ 1 1) 2)) (= (ite (< p i0) 1 1) (join p 0))))
(check-synth)
