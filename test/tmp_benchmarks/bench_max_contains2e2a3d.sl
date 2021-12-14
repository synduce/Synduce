(set-logic LIA)
(synth-fun c0 ((x8 Int) (x9 Int)) Bool ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool ((not Ipred) (and Ipred Ipred) (or Ipred Ipred) (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int (Ic x8 x9 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var b1 Bool)
(declare-var b0 Bool)
(declare-var p Int)
(declare-var i Int)
(declare-var x Int)
(constraint (or (not (> x i)) (= (or (= x p) (or b0 b1)) (c0 x p))))
(check-synth)
