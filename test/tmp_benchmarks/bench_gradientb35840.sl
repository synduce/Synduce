(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Int
 ((mk_synd_tup_Bool_Int (proj_synd_tup_Bool_Int_0 Bool) (proj_synd_tup_Bool_Int_1 Int))))
(synth-fun j0$1 ((x26 Int) (x27 synd_tup_Bool_Int) (x28 synd_tup_Bool_Int)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x26 (proj_synd_tup_Bool_Int_1 x27) (proj_synd_tup_Bool_Int_1 x28) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Int_0 x27) (proj_synd_tup_Bool_Int_0 x28) (= Ix Ix) 
    (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(check-synth)
