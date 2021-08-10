(set-logic DTLIA)
(synth-fun join0 ((x3 Int) (x4 (Tuple Int Int)) (x5 (Tuple Int Int))) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic x3 ((_ tupSel 0) x4) ((_ tupSel 1) x4) ((_ tupSel 0) x5) ((_ tupSel 1) x5) (- Ix) (+ Ix Ix)))
  (Ic Int ((Constant Int)))))
(check-synth)
