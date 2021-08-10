(set-logic DTLIA)
(synth-fun join1 ((x6 Int) (x7 (Tuple Int Int)) (x8 (Tuple Int Int))) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic x6 ((_ tupSel 0) x7) ((_ tupSel 1) x7) ((_ tupSel 0) x8) ((_ tupSel 1) x8) (- Ix) (+ Ix Ix)))
  (Ic Int ((Constant Int)))))
(check-synth)
