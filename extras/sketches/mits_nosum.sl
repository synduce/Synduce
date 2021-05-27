(set-logic DTLIA)

(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))

(synth-fun join1 ((x59 Int) (x60 Int) (x61 Int) (x62 Int) (x63 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x59 x60 x61 x62 x63 (- Ix) (+ Ix Ix) (max Ix Ix))) (Ic Int ((Constant Int)))))

(synth-fun expr0
  ()
  Int
  (
    (Ix Int)
    (Ic Int)
  )
  (
    (Ix Int (Ic (+ Ix Ix)))
    (Ic Int ((Constant Int)))
  )
)

(synth-fun expr1
  ((x59 Int))
  Int
  (
    (Ix Int)
    (Ic Int)
  )
  (
    (Ix Int (Ic x59 (- Ix) (+ Ix Ix) (max Ix Ix)))
    (Ic Int ((Constant Int)))
  )
)



(declare-var new Int)
(declare-var new29 Int)
(declare-var i_57 Int)
(declare-var i_58 Int)

(constraint
  (or (not (>= i_57 (- 0)))
  (=
    (max (+ i_57 new) (- 0))
    (join1 new i_57 i_58 (- 0) expr0))))

(constraint
 (or (not (>= i_57 (- 0)))
  (=
    (max (+ (max (+ i_57 new) (- 0)) new29) (- 0))
    (join1 new i_57 i_58 (max new29 (- 0)) (expr1 new29)))))

(check-synth)
