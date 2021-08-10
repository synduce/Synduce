(set-logic DTNIA)
(synth-fun join1 ((x16 Int) (x17 Int) (x18 (Tuple Int Int))) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x16 x17 ((_ tupSel 0) x18) ((_ tupSel 1) x18) (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i Int)
(declare-var i2 Int)
(declare-var i4 Int)
(constraint
 (or (not (not (= i2 i))) (= (ite (> (+ (ite (= i2 i) 1 0) 1) 1) i i2) (join1 i 1 (mkTuple 1 i2)))))
(constraint
 (or (not (not (= i4 i)))
  (=
   (ite
    (> (+ (+ (ite (= i4 i) 1 0) 1) 1)
     (ite (> (+ (ite (= i4 i) 1 0) 1) 1) (+ (ite (= i4 i) 1 0) 1) 1))
    i (ite (> (+ (ite (= i4 i) 1 0) 1) 1) i i4))
   (join1 i (+ 1 1) (mkTuple 1 i4)))))
(check-synth)
