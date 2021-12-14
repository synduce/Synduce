(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun odot$0 ((x34 synd_tup_Int_Int) (x35 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x34) (proj_synd_tup_Int_Int_1 x34) (proj_synd_tup_Int_Int_0 x35)
    (proj_synd_tup_Int_Int_1 x35) (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun s0$1 () Int)
(declare-var i Int)
(declare-var x Int)
(declare-var _elim_i0 Int)
(declare-var _elim_i Int)
(constraint
 (= _elim_i (odot$0 (mk_synd_tup_Int_Int 0 s0$1) (mk_synd_tup_Int_Int _elim_i _elim_i0))))
(constraint
 (= (+ (* x _elim_i) i)
  (odot$0 (mk_synd_tup_Int_Int (+ (* x 0) i) x) (mk_synd_tup_Int_Int _elim_i _elim_i0))))
(check-synth)
