(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun join$0 ((x6 Int) (x7 synd_tup_Int_Int) (x8 synd_tup_Int_Int)) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic x6 (proj_synd_tup_Int_Int_0 x7) (proj_synd_tup_Int_Int_1 x7) (proj_synd_tup_Int_Int_0 x8)
    (proj_synd_tup_Int_Int_1 x8) (- Ix) (+ Ix Ix)))
  (Ic Int ((Constant Int)))))
(check-synth)
