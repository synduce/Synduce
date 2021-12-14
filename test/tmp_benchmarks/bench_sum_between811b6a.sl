(set-logic LIA)
(synth-fun f_a_gt_hi ((x25 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix))) (Ix Int (Ic x25 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i1082 Int)
(declare-var i1081 Int)
(declare-var i Int)
(declare-var lo Int)
(declare-var hi Int)
(constraint
 (or
  (not
   (and (and (and (and (>= i1081 0) (>= i1082 0)) (or (not (< i lo)) (= i1081 0))) (not (< i lo)))
    (> i hi)))
  (= (ite (and (> hi i) (> i lo)) (+ (+ i i1081) i1082) (+ i1081 i1082)) (f_a_gt_hi i1081))))
(check-synth)
