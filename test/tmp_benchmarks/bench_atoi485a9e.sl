(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun odot$1 ((x2 synd_tup_Int_Int) (x3 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((* Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x2) (proj_synd_tup_Int_Int_1 x2) (proj_synd_tup_Int_Int_0 x3)
    (proj_synd_tup_Int_Int_1 x3) (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i Int)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint (= i1 (odot$1 (mk_synd_tup_Int_Int 0 1) (mk_synd_tup_Int_Int i0 i1))))
(constraint
 (= (* 10 i1) (odot$1 (mk_synd_tup_Int_Int (+ (* 10 0) i) (* 10 1)) (mk_synd_tup_Int_Int i0 i1))))
(check-synth)
