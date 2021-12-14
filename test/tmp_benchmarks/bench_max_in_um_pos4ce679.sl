(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join ((x44 Int) (x45 Int) (x46 Int) (x47 Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int (Ic x44 x45 x46 x47 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i4 Int)
(declare-var i2984 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and
    (and (and (>= i i) (ite (= i 0) (= i 0) (> i 0)))
     (and (>= i i) (ite (> i i0) (> i0 i2984) (= i i))))
    (and (>= i i) (ite (> i i0) (> i i2984) (= i i)))))
  (= (max i (max i0 i2984)) (join i i0 0 i2984))))
(constraint
 (or
  (not
   (and
    (and (and (>= i i) (ite (= i 0) (= i 0) (> i 0)))
     (and (>= i i) (ite (>= i i0) (> i0 i2984) (> i i4))))
    (and (>= i i) (ite (>= i i0) (> i i2984) (= i i)))))
  (= (max i4 (max i (max i0 i2984))) (join i i0 (max i4 0) i2984))))
(check-synth)
