(set-logic LIA)
(synth-fun xi_2 ((x21 Int) (x22 Int) (x23 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x21 x22 x23 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i250 Int)
(declare-var i249 Int)
(declare-var i Int)
(declare-var x Int)
(constraint
 (or
  (not
   (and
    (and (and (and (>= i249 0) (<= i249 1)) (and (>= i250 0) (<= i250 1)))
     (or (not (< x i)) (= i250 0)))
    (not (< x i))))
  (= (ite (= i x) 1 (ite (= i249 1) 1 (ite (= i250 1) 1 0))) (xi_2 x i i250))))
(check-synth)
