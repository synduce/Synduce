(set-logic DTNIA)
(synth-fun join0 ((x3 Int) (x4 Int) (x5 (Tuple Int Int))) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x3 x4 ((_ tupSel 0) x5) ((_ tupSel 1) x5) (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i Int)
(declare-var i2 Int)
(constraint
 (or (not (not (= i2 i)))
  (= (ite (> (+ (ite (= i2 i) 1 0) 1) 1) (+ (ite (= i2 i) 1 0) 1) 1) (join0 i 1 (mkTuple 1 i2)))))
(check-synth)