(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun odot$0 ((x6 synd_tup_Int_Int) (x7 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((+ Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x6) (proj_synd_tup_Int_Int_1 x6) (proj_synd_tup_Int_Int_0 x7)
    (proj_synd_tup_Int_Int_1 x7) (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i Int)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint (= i0 (odot$0 (mk_synd_tup_Int_Int 0 1) (mk_synd_tup_Int_Int i0 i1))))
(constraint
 (= (+ (* 10 i0) i)
  (odot$0 (mk_synd_tup_Int_Int (+ (* 10 0) i) (* 10 1)) (mk_synd_tup_Int_Int i0 i1))))
(check-synth)
