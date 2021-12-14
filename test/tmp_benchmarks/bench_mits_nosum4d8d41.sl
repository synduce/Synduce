(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join1$1 ((x165 Int) (x166 synd_tup_Int_Int) (x167 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((+ Ix Ix)))
  (Ix Int
   (Ic x165 (proj_synd_tup_Int_Int_0 x166) (proj_synd_tup_Int_Int_1 x166)
    (proj_synd_tup_Int_Int_0 x167) (proj_synd_tup_Int_Int_1 x167) (- Ix) 
    (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var p8 Int)
(declare-var p5 Int)
(declare-var p2 Int)
(constraint
 (= (+ p2 p5) (join1$1 p2 (mk_synd_tup_Int_Int (max (+ 0 p5) 0) p5) (mk_synd_tup_Int_Int 0 0))))
(constraint
 (= (+ p2 p8) (join1$1 p2 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int (max (+ 0 p8) 0) p8))))
(constraint (= p2 (join1$1 p2 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int 0 0))))
(check-synth)
