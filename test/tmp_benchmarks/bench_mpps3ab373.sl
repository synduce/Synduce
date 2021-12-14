(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join1$1 ((x2 Int) (x3 synd_tup_Int_Int) (x4 synd_tup_Int_Int)) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic x2 (proj_synd_tup_Int_Int_0 x3) (proj_synd_tup_Int_Int_1 x3) (proj_synd_tup_Int_Int_0 x4)
    (proj_synd_tup_Int_Int_1 x4) (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var p7 Int)
(declare-var p0 Int)
(constraint (= (max 0 p0) (join1$1 p0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int 0 0))))
(constraint
 (= (max (max 0 p0) (+ p0 p7))
  (join1$1 p0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int (+ 0 p7) (max 0 (+ 0 p7))))))
(check-synth)
