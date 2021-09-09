(set-logic DTLIA)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join ((x144 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x144 (- Ix) (+ Ix Ix) (max Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var p Int)
(declare-var i42 Int)
(constraint
 (or
  (not (and (or (not (= (ite (> i42 p) i42 0) 0)) (= p (max p i42))) (= (ite (> i42 p) i42 0) 0)))
  (= (max p i42) (join p))))
(check-synth)
