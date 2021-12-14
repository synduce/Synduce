(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun base_case$1 () Int)
(synth-fun oplus$0 ((x20 Int) (x21 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic x20 (proj_synd_tup_Int_Int_0 x21) (proj_synd_tup_Int_Int_1 x21) 
    (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun oplus$1 ((x22 Int) (x23 synd_tup_Int_Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic x22 (proj_synd_tup_Int_Int_0 x23) (proj_synd_tup_Int_Int_1 x23) 
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
