(set-logic DTLIA)

(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))

(synth-fun odot_0 ((x48_0 Int) (x46_1 Int) (x49_0 Int) (x49_1 Int)) Int
 ((Ix Int) (Ic Int))
 ((Ix Int (Ic (Variable Int) (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int (0 1))))

(synth-fun odot_1 ((x48_0 Int) (x46_1 Int) (x49_0 Int) (x49_1 Int)) Int
 ((Ix Int) (Ic Int))
 ((Ix Int (Ic (Variable Int) (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int (0 1))))

(declare-var ex42 Int)
(declare-var i_ Int)
(declare-var i_46 Int)
(constraint (or (not (and (>= i_ 0) (>= i_ i_46))) (= i_ (odot_0 0 0 i_ i_46))))
(constraint (or (not (and (>= i_ 0)  (>= i_ i_46))) (= i_46 (odot_1 0 0 i_ i_46))))
(constraint (or (not (and (>= i_ 0) (>= i_ i_46))) (= (max (+ i_46 ex42) i_) (odot_0 (max ex42 0) ex42 i_ i_46))))

(constraint (or (not (>= i_ 0)) (= (+ i_46 ex42)
      (odot_1 (max ex42 0) ex42 i_ i_46))))

(check-synth)
