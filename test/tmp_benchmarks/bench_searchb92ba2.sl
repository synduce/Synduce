(set-logic LIA)
(synth-fun odot ((x15 Int) (x16 Int) (x17 Bool) (x18 Bool)) Bool
 ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((ite Ipred Ipred Ipred)))
  (Ipred Bool
   (x17 x18 (not Ipred) (and Ipred Ipred) (or Ipred Ipred) (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int (Ic x15 x16 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var i Int)
(declare-var b3 Bool)
(declare-var p Int)
(declare-var x Int)
(constraint (= (ite (= p x) true b3) (odot x p b3 false)))
(constraint (= (ite (= i x) true (ite (= p x) true b3)) (odot x p b3 (ite (= i x) true false))))
(check-synth)
