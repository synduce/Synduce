(set-logic LIA)
(synth-fun j0 ((x1 Int) (x2 Bool) (x3 Bool)) Bool ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   (x2 x3 (not Ipred) (and Ipred Ipred) (or Ipred Ipred) (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int (Ic x1 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var p10 Int)
(declare-var p6 Int)
(declare-var p Int)
(constraint (= (and (> p p6) (and (> p p10) (and (> p6 0) (> p10 0)))) (j0 p (> p6 0) (> p10 0))))
(check-synth)
