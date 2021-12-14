(set-logic LIA)
(synth-fun le_case ((x6 Int) (x7 Int) (x8 Bool) (x9 Bool)) Bool
 ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   (x8 x9 (not Ipred) (and Ipred Ipred) (or Ipred Ipred) (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int (Ic x6 x7 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var b1 Bool)
(declare-var b0 Bool)
(declare-var i Int)
(declare-var hi Int)
(declare-var lo Int)
(constraint
 (or (not (not (>= i hi))) (= (or (and (< lo i) (< i hi)) (or b0 b1)) (le_case i lo b0 b1))))
(check-synth)
