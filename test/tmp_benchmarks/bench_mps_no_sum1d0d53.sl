(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun join$1 ((x21 synd_tup_Int_Int) (x22 synd_tup_Int_Int)) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x21) (proj_synd_tup_Int_Int_1 x21) (proj_synd_tup_Int_Int_0 x22)
    (proj_synd_tup_Int_Int_1 x22) (- Ix) (+ Ix Ix)))
  (Ic Int ((Constant Int)))))
(check-synth)
