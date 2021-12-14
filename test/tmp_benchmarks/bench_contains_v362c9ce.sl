(set-logic LIA)
(synth-fun xi_2 ((x46 Int) (x47 Int) (x48 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x46 x47 x48 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i1068 Int)
(declare-var i1067 Int)
(declare-var i Int)
(declare-var x Int)
(constraint
 (or
  (not
   (and
    (and (and (and (>= i1067 0) (<= i1067 1)) (and (>= i1068 0) (<= i1068 1)))
     (and (or (not (not (< x i))) (= x (ite (= i1067 0) x i)))
      (and (or (not (< x i)) (= i1068 0)) (or (not (< x i)) (= i1068 0)))))
    (not (< x i))))
  (= (ite (= i x) 1 (ite (= i1067 1) 1 (ite (= i1068 1) 1 0))) (xi_2 x i i1068))))
(check-synth)
