(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun join$1 ((x17 Int) (x18 synd_tup_Int_Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x17 (proj_synd_tup_Int_Int_0 x18) (proj_synd_tup_Int_Int_1 x18) (- Ix) (+ Ix Ix)))
  (Ic Int ((Constant Int)))))
(check-synth)
