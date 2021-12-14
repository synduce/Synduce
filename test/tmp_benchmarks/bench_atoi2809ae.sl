(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun odot$1 ((x4 synd_tup_Int_Int) (x5 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((* Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x4) (proj_synd_tup_Int_Int_1 x4) (proj_synd_tup_Int_Int_0 x5)
    (proj_synd_tup_Int_Int_1 x5) (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i Int)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint (= i1 (odot$1 (mk_synd_tup_Int_Int 0 1) (mk_synd_tup_Int_Int i0 i1))))
(constraint
 (= (* 10 i1) (odot$1 (mk_synd_tup_Int_Int (+ (* 10 0) i) (* 10 1)) (mk_synd_tup_Int_Int i0 i1))))
(check-synth)
