(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun f$0 ((x22 Int) (x23 synd_tup_Int_Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x22 (proj_synd_tup_Int_Int_0 x23) (proj_synd_tup_Int_Int_1 x23) 
    (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun f$1 ((x24 Int) (x25 synd_tup_Int_Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x24 (proj_synd_tup_Int_Int_0 x25) (proj_synd_tup_Int_Int_1 x25) 
    (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i1 Int)
(declare-var i0 Int)
(constraint (= 0 (f$0 i0 (mk_synd_tup_Int_Int 0 1))))
(constraint
 (= i0 (f$0 i0 (mk_synd_tup_Int_Int (+ (* i1 0) 0) (f$1 i1 (mk_synd_tup_Int_Int 0 1))))))
(check-synth)
