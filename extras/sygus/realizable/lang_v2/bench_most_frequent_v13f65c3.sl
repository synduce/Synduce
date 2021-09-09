(set-logic DTNIA)
(synth-fun join0 ((x13 Int) (x14 (Tuple Int Int)) (x15 (Tuple Int Int))) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x13 ((_ tupSel 0) x14) ((_ tupSel 1) x14) ((_ tupSel 0) x15) ((_ tupSel 1) x15) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i Int)
(declare-var i2 Int)
(declare-var i4 Int)
(constraint
 (or (not (and (< i4 i) (<= i i2)))
  (= (ite (> (+ (ite (= i4 i2) 1 0) 1) 1) (+ (ite (= i4 i2) 1 0) 1) 1)
   (join0 i (mkTuple 1 i4) (mkTuple 1 i2)))))
(check-synth)
