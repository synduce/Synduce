(set-logic DTNIA)
(synth-fun join0 ((x23 Int) (x24 Int) (x25 (Tuple Int Int))) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x23 x24 ((_ tupSel 0) x25) ((_ tupSel 1) x25) (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i Int)
(declare-var i2 Int)
(declare-var i4 Int)
(constraint
 (or (not (not (= i2 i)))
  (= (ite (> (+ (ite (= i2 i) 1 0) 1) 1) (+ (ite (= i2 i) 1 0) 1) 1) (join0 i 1 (mkTuple 1 i2)))))
(constraint
 (or (not (not (= i2 i)))
  (=
   (ite (> (+ (+ (ite (= i2 i) 1 0) (ite (= i2 i) 1 0)) 1) (ite (> 2 1) 2 1))
    (+ (+ (ite (= i2 i) 1 0) (ite (= i2 i) 1 0)) 1) (ite (> 2 1) 2 1))
   (join0 i 1
    (mkTuple (ite (> (+ (ite (= i2 i2) 1 0) 1) 1) (+ (ite (= i2 i2) 1 0) 1) 1)
     (ite (> (+ (ite (= i2 i2) 1 0) 1) 1) i2 i2))))))
(constraint
 (or (not (not (= i4 i)))
  (=
   (ite
    (> (+ (+ (ite (= i4 i) 1 0) 1) 1)
     (ite (> (+ (ite (= i4 i) 1 0) 1) 1) (+ (ite (= i4 i) 1 0) 1) 1))
    (+ (+ (ite (= i4 i) 1 0) 1) 1) (ite (> (+ (ite (= i4 i) 1 0) 1) 1) (+ (ite (= i4 i) 1 0) 1) 1))
   (join0 i (+ 1 1) (mkTuple 1 i4)))))
(check-synth)
