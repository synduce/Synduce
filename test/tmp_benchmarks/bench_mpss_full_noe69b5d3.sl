(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun inner1 ((x22 Int) (x23 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x22 x23 (- Ix) (+ Ix Ix) (max Ix Ix))) (Ic Int ((Constant Int)))))
(synth-fun inner0 ((x24 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x24 (- Ix) (+ Ix Ix) (max Ix Ix))) (Ic Int ((Constant Int)))))
(synth-fun s0$0 ((x25 Int)) Int ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((max Ix Ix))) (Ix Int (Ic x25 (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(synth-fun s0$1 ((x26 Int)) Int ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((+ Ix Ix))) (Ix Int (Ic x26 (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
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
