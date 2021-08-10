(set-logic LIA)
(synth-fun c0 ((x81 Int) (x82 Int)) Bool
 ((Start Bool ((not Start) (and Start Start) (or Start Start) (= Ix Ic) (= Ix Ix) (> Ix Ix)))
  (Ix Int (Ic x81 x82 (- Ix) (+ Ix Ix) (+ Ix Ic) (ite Start Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var x Int)
(declare-var i Int)
(declare-var p Int)
(declare-var b5 Bool)
(declare-var b6 Bool)
(constraint
 (or
  (not
   (and
    (and (or (not (and (or (not (> x i)) (> x p)) (> x i))) (> x (ite b5 x p)))
     (or (not (> x i)) (> x p)))
    (> x i)))
  (= (or (= x p) (or b5 b6)) (c0 x p))))
(check-synth)
