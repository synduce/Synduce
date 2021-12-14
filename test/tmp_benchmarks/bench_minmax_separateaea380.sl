(set-logic LIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun xi_2$1 ((x50 Int) (x51 Int) (x52 Int) (x53 Int) (x54 Int)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((min Ix Ix)))
  (Ix Int (Ic x50 x51 x52 x53 x54 (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i12 Int)
(declare-var i10 Int)
(declare-var i1 Int)
(declare-var i8 Int)
(declare-var i6 Int)
(declare-var i3 Int)
(declare-var i4 Int)
(declare-var i2 Int)
(declare-var i Int)
(constraint (= (min i2 (min i4 i)) (xi_2$1 i i2 i4 i2 i4)))
(constraint
 (= (min i2 (min (min i6 (min i8 i3)) i))
  (xi_2$1 i i2 (max i3 (max i6 i8)) i2 (min i3 (min i6 i8)))))
(constraint
 (= (min (min i10 (min i12 i1)) (min i4 i))
  (xi_2$1 i (min i1 (min i10 i12)) i4 (max i1 (max i10 i12)) i4)))
(check-synth)
