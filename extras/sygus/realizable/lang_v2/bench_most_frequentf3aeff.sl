(set-logic DTNIA)
(synth-fun join1 ((x6 Int) (x7 Int) (x8 (Tuple Int Int))) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x6 x7 ((_ tupSel 0) x8) ((_ tupSel 1) x8) (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i Int)
(declare-var i2 Int)
(constraint
 (or (not (not (= i2 i))) (= (ite (> (+ (ite (= i2 i) 1 0) 1) 1) i i2) (join1 i 1 (mkTuple 1 i2)))))
(check-synth)
