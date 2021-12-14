(set-logic LIA)
(synth-fun odot ((x11 Int) (x12 Int) (x13 Bool) (x14 Bool)) Bool
 ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((ite Ipred Ipred Ipred)))
  (Ipred Bool
   (x13 x14 (not Ipred) (and Ipred Ipred) (or Ipred Ipred) (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int (Ic x11 x12 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var i Int)
(declare-var b3 Bool)
(declare-var p Int)
(declare-var x Int)
(constraint (= (ite (= p x) true b3) (odot x p false b3)))
(constraint (= (ite (= i x) true (ite (= p x) true b3)) (odot x p (ite (= i x) true false) b3)))
(check-synth)
