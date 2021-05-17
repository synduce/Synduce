(set-logic DTLIA)

(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))

(synth-fun join1_0 ((x59 Int) (x60 Int) (x61 Int) (x62 Int) (x63 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x59 x60 x61 x62 x63 (- Ix) (+ Ix Ix) (max Ix Ix))) (Ic Int ((Constant Int)))))

(synth-fun join1_1 ((x59 Int) (x60 Int) (x61 Int) (x62 Int) (x63 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x59 x60 x61 x62 x63 (- Ix) (+ Ix Ix) (max Ix Ix))) (Ic Int ((Constant Int)))))

(declare-var n Int)
(declare-var n3 Int)
(declare-var n4 Int)
(declare-var i1 Int)
(declare-var i2 Int)

(constraint
  (or (not (>= i1 (- 0)))
  (=
    (max (+ i1 n) (- 0))
    (join1_0 n i1 i2 (- 0) 0))))

(constraint
 (or (not (>= i1 (- 0)))
  (=
    (max (+ (max (+ i1 n) (- 0)) n3) (- 0))
    (join1_0 n i1 i2 (max n3 (- 0)) n3))))

(constraint
 (or (not (>= i1 (- 0)))
  (=
    (max (+ (max (+ (max (+ i1 n) (- 0)) n3) (- 0)) n4) 0)
    (join1_0 n i1 i2 (max (+ (max n3 0) n4) 0) (+ n3 n4)))))

(constraint
 (or (not (>= i1 (- 0)))
  (= n3 (join1_1 n3 0 0 0 0))))

(constraint
 (or (not (>= i1 (- 0)))
  (=
    (+ n3 n4)
    (join1_1 n3 (max n4 0) n4 0 0)
    )))


(check-synth)
