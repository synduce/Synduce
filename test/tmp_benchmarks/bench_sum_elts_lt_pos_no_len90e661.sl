(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun f$0 ((x30 Int) (x31 synd_tup_Int_Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic x30 (proj_synd_tup_Int_Int_0 x31) (proj_synd_tup_Int_Int_1 x31) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun f$1 ((x32 Int) (x33 synd_tup_Int_Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x32 (proj_synd_tup_Int_Int_0 x33) (proj_synd_tup_Int_Int_1 x33) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun s0$1 () Int)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint (= (ite (> i0 0) i0 0) (f$0 i0 (mk_synd_tup_Int_Int 0 s0$1))))
(constraint
 (= (ite (> i0 1) (+ i0 (ite (> i1 0) i1 0)) (ite (> i1 0) i1 0))
  (f$0 i0 (mk_synd_tup_Int_Int (ite (> i1 0) (+ i1 0) 0) (f$1 i1 (mk_synd_tup_Int_Int 0 s0$1))))))
(check-synth)
