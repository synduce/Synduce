(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun s0$1 () Int)
(synth-fun join$0 ((x35 Int) (x36 Int) (x37 synd_tup_Int_Int) (x38 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((* Ix Ix)))
  (Ix Int
   (Ic x35 x36 (proj_synd_tup_Int_Int_0 x37) (proj_synd_tup_Int_Int_1 x37)
    (proj_synd_tup_Int_Int_0 x38) (proj_synd_tup_Int_Int_1 x38) (- Ix) 
    (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
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
