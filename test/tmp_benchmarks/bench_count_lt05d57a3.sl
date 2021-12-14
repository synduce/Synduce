(set-logic LIA)
(synth-fun rec_stop ((x23 Int) (x24 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x23 x24 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i556 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and (and (or (not (< i 0)) (= i0 (+ i556 1))) (or (not (< i 0)) (>= i556 (+ i i0)))) (< i 0)))
  (= (+ (ite (< i 0) 1 0) i556) (rec_stop i i0))))
(check-synth)
