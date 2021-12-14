(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun inner1 ((x27 Int) (x28 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x27 x28 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix))) 
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun inner0 ((x29 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x29 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun s0$0 ((x30 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix))) (Ix Int (Ic x30 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun s0$1 ((x31 Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x31 (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i8 Int)
(declare-var i Int)
(declare-var i10 Int)
(declare-var i9 Int)
(declare-var i2 Int)
(declare-var i0 Int)
(constraint (= (max 0 i0) (s0$0 (inner0 i0))))
(constraint (= i0 (s0$1 (inner0 i0))))
(constraint (= (max (+ i10 i2) i9) (max i9 (+ i10 i2))))
(constraint (= (+ i10 i2) (+ i10 i2)))
(constraint (= (max 0 (+ i i8)) (s0$0 (inner1 i (inner0 i8)))))
(constraint (= (+ i i8) (s0$1 (inner1 i (inner0 i8)))))
(check-synth)
