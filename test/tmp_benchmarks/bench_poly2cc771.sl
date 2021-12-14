(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun join$1 ((x16 Int) (x17 Int) (x18 synd_tup_Int_Int) (x19 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((* Ix Ix)))
  (Ix Int
   (Ic x16 x17 (proj_synd_tup_Int_Int_0 x18) (proj_synd_tup_Int_Int_1 x18)
    (proj_synd_tup_Int_Int_0 x19) (proj_synd_tup_Int_Int_1 x19) (- Ix) 
    (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var p2 Int)
(declare-var i8 Int)
(declare-var i7 Int)
(declare-var p Int)
(declare-var poly_in Int)
(constraint
 (= (* poly_in i8) (join$1 poly_in p (mk_synd_tup_Int_Int i7 i8) (mk_synd_tup_Int_Int 0 1))))
(constraint
 (= (* poly_in (* poly_in i8))
  (join$1 poly_in p (mk_synd_tup_Int_Int i7 i8) (mk_synd_tup_Int_Int (+ 0 (* 1 p2)) (* poly_in 1)))))
(check-synth)
