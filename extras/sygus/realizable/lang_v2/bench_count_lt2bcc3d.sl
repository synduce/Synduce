(set-logic DTLIA)
(synth-fun f1 ((x133 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x133 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var i Int)
(declare-var i0 Int)
(declare-var i27597 Int)
(declare-var i27598 Int)
(constraint
 (or
  (not
   (and
    (and (and (>= i27597 0) (>= i27598 0))
     (and
      (or
       (not
        (and
         (and (and (>= i27597 0) (>= i27598 0))
          (or (not (and (and (>= i27597 0) (>= i27598 0)) (< i0 2))) (= i (+ (+ i27597 i27598) 1))))
         (not (< i0 2))))
       (= i (+ i27597 i27598)))
      (or (not (and (and (>= i27597 0) (>= i27598 0)) (< i0 2))) (= i (+ (+ i27597 i27598) 1)))))
    (not (< i0 2))))
  (= (+ i27597 i27598) (f1 i))))
(check-synth)
