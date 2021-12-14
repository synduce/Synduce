(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun base_case$1 () Int)
(synth-fun oplus$0 ((x32 Int) (x33 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic x32 (proj_synd_tup_Int_Int_0 x33) (proj_synd_tup_Int_Int_1 x33) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun oplus$1 ((x34 Int) (x35 synd_tup_Int_Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x34 (proj_synd_tup_Int_Int_0 x35) (proj_synd_tup_Int_Int_1 x35) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or (not (and (> i 0) (and true true))) (= 0 (oplus$0 i (mk_synd_tup_Int_Int 0 base_case$1)))))
(constraint
 (or (not (and (> i 0) (and (and (> i0 0) (and true true)) (and (not (= i i0)) true))))
  (= (ite (> i i0) i0 i) (oplus$0 i (mk_synd_tup_Int_Int 0 i0)))))
(constraint
 (or (not (and (> i0 0) (and true true))) (= i0 (oplus$1 i0 (mk_synd_tup_Int_Int 0 base_case$1)))))
(check-synth)
