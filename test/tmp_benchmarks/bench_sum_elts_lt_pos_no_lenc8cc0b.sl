(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun f$0 ((x18 Int) (x19 synd_tup_Int_Int)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic x18 (proj_synd_tup_Int_Int_0 x19) (proj_synd_tup_Int_Int_1 x19) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun f$1 ((x20 Int) (x21 synd_tup_Int_Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x20 (proj_synd_tup_Int_Int_0 x21) (proj_synd_tup_Int_Int_1 x21) 
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
