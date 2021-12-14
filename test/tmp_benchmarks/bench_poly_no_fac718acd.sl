(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun odot$1 ((x24 synd_tup_Int_Int) (x25 synd_tup_Int_Int)) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x24) (proj_synd_tup_Int_Int_1 x24) (proj_synd_tup_Int_Int_0 x25)
    (proj_synd_tup_Int_Int_1 x25) (- Ix) (+ Ix Ix)))
  (Ic Int ((Constant Int)))))
(check-synth)
