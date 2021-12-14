(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun join$1 ((x27 Int) (x28 Int) (x29 synd_tup_Int_Int) (x30 synd_tup_Int_Int)) Int
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic x27 x28 (proj_synd_tup_Int_Int_0 x29) (proj_synd_tup_Int_Int_1 x29)
    (proj_synd_tup_Int_Int_0 x30) (proj_synd_tup_Int_Int_1 x30) (- Ix) 
    (+ Ix Ix)))
  (Ic Int ((Constant Int)))))
(check-synth)
