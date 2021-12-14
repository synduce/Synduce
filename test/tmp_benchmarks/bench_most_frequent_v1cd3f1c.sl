(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun join$1 ((x0 Int) (x1 synd_tup_Int_Int) (x2 synd_tup_Int_Int)) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic x0 (proj_synd_tup_Int_Int_0 x1) (proj_synd_tup_Int_Int_1 x1) (proj_synd_tup_Int_Int_0 x2)
    (proj_synd_tup_Int_Int_1 x2) (- Ix) (+ Ix Ix)))
  (Ic Int ((Constant Int)))))
(check-synth)
