(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun s0$1 () Int)
(synth-fun join$0 ((x47 Int) (x48 Int) (x49 synd_tup_Int_Int) (x50 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((* Ix Ix)))
  (Ix Int
   (Ic x47 x48 (proj_synd_tup_Int_Int_0 x49) (proj_synd_tup_Int_Int_1 x49)
    (proj_synd_tup_Int_Int_0 x50) (proj_synd_tup_Int_Int_1 x50) (- Ix) 
    (+ Ix Ix) (* Ix Ix) (div Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i1 Int)
(declare-var _elim_i0 Int)
(declare-var _elim_i Int)
(declare-var p1 Int)
(declare-var p0 Int)
(constraint
 (= (* p0 (* p1 _elim_i))
  (join$0 p0 p1 (mk_synd_tup_Int_Int 0 s0$1) (mk_synd_tup_Int_Int _elim_i _elim_i0))))
(constraint
 (= (* i1 (* p0 (* p1 _elim_i)))
  (join$0 p0 p1 (mk_synd_tup_Int_Int (* i1 0) i1) (mk_synd_tup_Int_Int _elim_i _elim_i0))))
(check-synth)
