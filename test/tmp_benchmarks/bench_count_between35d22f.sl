(set-logic LIA)
(synth-fun le_case ((x10 Int) (x11 Int) (x12 Bool) (x13 Bool)) Bool
 ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   (x12 x13 (not Ipred) (and Ipred Ipred) (or Ipred Ipred) (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int (Ic x10 x11 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var b1 Bool)
(declare-var b0 Bool)
(declare-var i Int)
(declare-var hi Int)
(declare-var lo Int)
(constraint
 (or (not (not (>= i hi))) (= (or (and (< lo i) (< i hi)) (or b0 b1)) (le_case i lo b0 b1))))
(check-synth)
