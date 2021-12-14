(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun odot$0 ((x31 synd_tup_Int_Int) (x32 synd_tup_Int_Int)) Int
 ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x31) (proj_synd_tup_Int_Int_1 x31) (proj_synd_tup_Int_Int_0 x32)
    (proj_synd_tup_Int_Int_1 x32) (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun s0$1 () Int)
(synth-fun f0$1 ((x33 Int)) Int ((Ix Int) (Ic Int) (Ipred Bool))
 ((Ix Int (Ic x33 (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i Int)
(declare-var _elim_i0 Int)
(declare-var _elim_i Int)
(constraint
 (= _elim_i (odot$0 (mk_synd_tup_Int_Int 0 s0$1) (mk_synd_tup_Int_Int _elim_i _elim_i0))))
(constraint
 (= (+ (* 10 _elim_i) i)
  (odot$0 (mk_synd_tup_Int_Int (+ (* 10 0) i) (f0$1 i)) (mk_synd_tup_Int_Int _elim_i _elim_i0))))
(check-synth)
