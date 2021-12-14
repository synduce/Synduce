(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun odot$0 ((x22 synd_tup_Int_Int) (x23 synd_tup_Int_Int)) Int 
 ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x22) (proj_synd_tup_Int_Int_1 x22) (proj_synd_tup_Int_Int_0 x23)
    (proj_synd_tup_Int_Int_1 x23) (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(synth-fun s0$1 () Int)
(synth-fun f0$1 ((x24 Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int (Ic x24 (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix))) (Ic Int ((Constant Int)))))
(declare-var i Int)
(declare-var _elim_i0 Int)
(declare-var _elim_i Int)
(constraint
 (= _elim_i (odot$0 (mk_synd_tup_Int_Int 0 s0$1) (mk_synd_tup_Int_Int _elim_i _elim_i0))))
(constraint
 (= (+ (* 10 _elim_i) i)
  (odot$0 (mk_synd_tup_Int_Int (+ (* 10 0) i) (f0$1 i)) (mk_synd_tup_Int_Int _elim_i _elim_i0))))
(check-synth)
