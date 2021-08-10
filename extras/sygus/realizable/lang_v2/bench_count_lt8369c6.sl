(set-logic DTLIA)
(synth-fun f0 ((x102 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x102 (- Ix) (+ Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var i Int)
(declare-var i0 Int)
(declare-var i17335 Int)
(declare-var i17336 Int)
(constraint
 (or
  (not
   (and
    (and (and (>= i17335 0) (>= i17336 0))
     (or (not (and (and (>= i17335 0) (>= i17336 0)) (< i0 2))) (= i (+ (+ i17335 i17336) 1))))
    (< i0 2)))
  (= (+ (+ 1 i17335) i17336) (f0 i))))
(check-synth)
