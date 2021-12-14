(set-logic LIA)
(synth-fun xi_1 ((x21 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x21 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i256 Int)
(declare-var i255 Int)
(declare-var i Int)
(declare-var x Int)
(constraint
 (or
  (not
   (and
    (and (and (and (>= i255 0) (<= i255 1)) (and (>= i256 0) (<= i256 1)))
     (or (not (< x i)) (= i256 0)))
    (< x i)))
  (= (ite (= i x) 1 (ite (= i255 1) 1 (ite (= i256 1) 1 0))) (xi_1 i255))))
(check-synth)
