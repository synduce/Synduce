(set-logic DTLIA)
(define-fun max ((x Int) (y Int)) Int (ite (> x y) x y))

(synth-fun odot_1 ((x52_0 Int) (x52_1 Int) (x52_2 Int) (x53_0 Int) (x53_1 Int) (x53_2 Int)) Int
 ((Ix Int) (Ic Int))
 ((Ix Int (Ic (Variable Int)  (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int (0 1))))

(synth-fun odot_2 ((x52_0 Int) (x52_1 Int) (x52_2 Int) (x53_0 Int) (x53_1 Int) (x53_2 Int)) Int
 ((Ix Int) (Ic Int))
 ((Ix Int (Ic (Variable Int)  (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int (0 1))))

(synth-fun odot_3 ((x52_0 Int) (x52_1 Int) (x52_2 Int) (x53_0 Int) (x53_1 Int) (x53_2 Int)) Int
 ((Ix Int) (Ic Int))
 ((Ix Int (Ic (Variable Int)  (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int (0 1))))


(declare-var ex44 Int)
(declare-var i_ Int)
(declare-var i_48 Int)
(declare-var i_50 Int)

(constraint
 (or (not (and (>= i_50 0) (and (>= i_48 0) (and (>= i_48 i_) (>= i_50 i_)))))
  (= i_ (odot_1 0 0 0 i_ i_48 i_50))))

(constraint
 (or (not (and (>= i_50 0) (and (>= i_48 0) (and (>= i_48 i_) (>= i_50 i_)))))
  (= i_48 (odot_2 0 0 0 i_ i_48 i_50))))

(constraint
 (or (not (and (>= i_50 0) (and (>= i_48 0) (and (>= i_48 i_) (>= i_50 i_)))))
  (= i_50 (odot_3 0 0 0 i_ i_48 i_50))))

(constraint
 (or (not (and (>= i_50 0) (and (>= i_48 0) (and (>= i_48 i_) (>= i_50 i_)))))
  (= (+ i_ ex44)
   (odot_1  ex44 (max ex44 0) (max ex44 0)  i_ i_48 i_50))))

(constraint
 (or (not (and (>= i_50 0) (and (>= i_48 0) (and (>= i_48 i_) (>= i_50 i_)))))
  (= (max (+ i_48 ex44) 0)
   (odot_2 ex44 (max ex44 0) (max ex44 0) i_ i_48 i_50))))

(constraint
 (or (not (and (>= i_50 0) (and (>= i_48 0) (and (>= i_48 i_) (>= i_50 i_)))))
  (= (max i_50 (+ i_ ex44))
   (odot_3 ex44 (max ex44 0) (max ex44 0) i_ i_48 i_50))))

(check-synth)
