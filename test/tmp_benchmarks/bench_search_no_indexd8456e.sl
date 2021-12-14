(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun f$0 ((x41 Int) (x42 Int) (x43 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic x41 x42 (proj_synd_tup_Int_Int_0 x43) (proj_synd_tup_Int_Int_1 x43) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun f$1 ((x44 Int) (x45 Int) (x46 synd_tup_Int_Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x44 x45 (proj_synd_tup_Int_Int_0 x46) (proj_synd_tup_Int_Int_1 x46) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun s0$1 () Int)
(declare-var i1 Int)
(declare-var i0 Int)
(declare-var x Int)
(constraint (= (ite (= x i0) 0 0) (f$0 x i0 (mk_synd_tup_Int_Int 0 s0$1))))
(constraint
 (= (ite (= x i0) 1 (ite (= x i1) 0 0))
  (f$0 x i0 (mk_synd_tup_Int_Int (ite (= x i1) 0 0) (f$1 x i1 (mk_synd_tup_Int_Int 0 s0$1))))))
(check-synth)
