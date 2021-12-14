(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Int
 ((mk_synd_tup_Bool_Int (proj_synd_tup_Bool_Int_0 Bool) (proj_synd_tup_Bool_Int_1 Int))))
(synth-fun j0$1 ((x23 Int) (x24 synd_tup_Bool_Int) (x25 synd_tup_Bool_Int)) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int (Ic x23 (proj_synd_tup_Bool_Int_1 x24) (proj_synd_tup_Bool_Int_1 x25) (- Ix) (+ Ix Ix)))
  (Ic Int ((Constant Int)))))
(check-synth)
