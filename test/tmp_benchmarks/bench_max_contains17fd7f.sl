(set-logic LIA)
(synth-fun c0 ((x52 Int) (x53 Int)) Bool ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool ((not Ipred) (and Ipred Ipred) (or Ipred Ipred) (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int (Ic x52 x53 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var b7 Bool)
(declare-var b6 Bool)
(declare-var p Int)
(declare-var i Int)
(declare-var x Int)
(constraint
 (or
  (not
   (and
    (and
     (and (or (not (> x i)) (> x (ite b6 x (ite b7 x p)))) (or (not (> x i)) (> x (ite b6 x p))))
     (or (not (> x i)) (> x p)))
    (> x i)))
  (= (or (= x p) (or b6 b7)) (c0 x p))))
(check-synth)
