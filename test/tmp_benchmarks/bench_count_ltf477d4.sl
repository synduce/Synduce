(set-logic LIA)
(synth-fun rec_cont ((x25 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x25 (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i556 Int)
(declare-var i0 Int)
(declare-var i Int)
(declare-var param Int)
(constraint
 (or
  (not
   (and (and (or (not (< i param)) (= i0 (+ i556 1))) (or (not (< i param)) (>= (+ i556 i556) i0)))
    (not (< i param))))
  (= (+ (ite (< i param) 1 (- 1)) i556) (rec_cont i556))))
(check-synth)
