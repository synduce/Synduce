(set-logic LIA)
(synth-fun oplus ((x1 Int) (x2 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x1 x2 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i0 Int)
(declare-var p Int)
(constraint
 (=
  (ite (> p i0) (ite (>= p 0) (+ p (ite (>= i0 0) i0 0)) 0)
   (ite (>= i0 0) (+ i0 (ite (>= p 0) p 0)) 0))
  (oplus p (ite (>= i0 0) i0 0))))
(check-synth)
