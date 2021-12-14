(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun join$0 ((x9 Int) (x10 synd_tup_Int_Int) (x11 synd_tup_Int_Int)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x9 (proj_synd_tup_Int_Int_0 x10) (proj_synd_tup_Int_Int_1 x10) 
    (proj_synd_tup_Int_Int_0 x11) (proj_synd_tup_Int_Int_1 x11) (- Ix) 
    (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(check-synth)
