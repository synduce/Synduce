(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun f$0 ((x73 Int) (x74 Int) (x75 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic x73 x74 (proj_synd_tup_Int_Int_0 x75) (proj_synd_tup_Int_Int_1 x75) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun f$1 ((x76 Int) (x77 Int) (x78 synd_tup_Int_Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x76 x77 (proj_synd_tup_Int_Int_0 x78) (proj_synd_tup_Int_Int_1 x78) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i2 Int)
(declare-var i1 Int)
(declare-var i0 Int)
(declare-var x Int)
(constraint (= (ite (= x i0) 0 0) (f$0 x i0 (mk_synd_tup_Int_Int 0 0))))
(constraint
 (= (ite (= x i0) 1 (ite (= x i1) 0 0))
  (f$0 x i0 (mk_synd_tup_Int_Int (ite (= x i1) 0 0) (f$1 x i1 (mk_synd_tup_Int_Int 0 0))))))
(constraint
 (= (ite (= x i0) 2 (ite (= x i1) 1 (ite (= x i2) 0 0)))
  (f$0 x i0
   (mk_synd_tup_Int_Int (ite (= x i1) (+ 1 0) (ite (= x i2) 0 0))
    (f$1 x i1
     (mk_synd_tup_Int_Int (f$0 x i2 (mk_synd_tup_Int_Int 0 0))
      (f$1 x i2 (mk_synd_tup_Int_Int 0 0))))))))
(check-synth)
