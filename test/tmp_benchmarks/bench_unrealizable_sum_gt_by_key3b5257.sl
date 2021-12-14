(set-logic LIA)
(synth-fun join2 ((x17 Int) (x18 Int) (x19 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x17 x18 x19 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i5 Int)
(declare-var i4 Int)
(declare-var i3 Int)
(declare-var i Int)
(declare-var key Int)
(constraint
 (or (not (not (> i key))) (= (ite (> i3 key) (+ i4 i5) i5) (join2 key i (ite (> i3 key) i4 0)))))
(check-synth)
