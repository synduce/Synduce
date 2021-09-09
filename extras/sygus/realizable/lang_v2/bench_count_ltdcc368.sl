(set-logic DTLIA)
(synth-fun f0 ((x134 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x134 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
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
    (< i0 2)))
  (= (+ (+ 1 i27597) i27598) (f0 i))))
(check-synth)
