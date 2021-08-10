(set-logic DTLIA)
(synth-fun xi_2 ((x19 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x19 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var x Int)
(declare-var i Int)
(declare-var i1957 Int)
(declare-var i1958 Int)
(constraint
 (or
  (not
   (and
    (and (and (>= i1957 0) (>= i1958 0))
     (or (not (and (and (>= i1957 0) (>= i1958 0)) (not (< i x)))) (= i1958 0)))
    (not (< i x))))
  (= (+ i1957 i1958) (xi_2 i1957))))
(check-synth)
