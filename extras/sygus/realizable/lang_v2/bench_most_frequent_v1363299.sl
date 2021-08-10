(set-logic DTNIA)
(synth-fun join1 ((x26 Int) (x27 (Tuple Int Int)) (x28 (Tuple Int Int))) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x26 ((_ tupSel 0) x27) ((_ tupSel 1) x27) ((_ tupSel 0) x28) ((_ tupSel 1) x28) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i Int)
(declare-var i2 Int)
(declare-var i4 Int)
(declare-var i8 Int)
(constraint
 (or (not (and (< i4 i) (<= i i2)))
  (= (ite (> (+ (ite (= i4 i2) 1 0) 1) 1) i2 i4) (join1 i (mkTuple 1 i4) (mkTuple 1 i2)))))
(constraint
 (or (not (and (< i8 i) (<= i i2)))
  (=
   (ite
    (> (+ (+ (ite (= i8 i2) 1 0) 1) 1)
     (ite (> (+ (ite (= i8 i2) 1 0) 1) 1) (+ (ite (= i8 i2) 1 0) 1) 1))
    i2 (ite (> (+ (ite (= i8 i2) 1 0) 1) 1) i2 i8))
   (join1 i (mkTuple 1 i8)
    (mkTuple (ite (> (+ (ite (= i2 i2) 1 0) 1) 1) (+ (ite (= i2 i2) 1 0) 1) 1)
     (ite (> (+ (ite (= i2 i2) 1 0) 1) 1) i2 i2))))))
(check-synth)
