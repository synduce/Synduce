(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun f$0 ((x54 Int) (x55 synd_tup_Int_Int)) Int ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((+ Ix Ix)))
  (Ix Int
   (Ic x54 (proj_synd_tup_Int_Int_0 x55) (proj_synd_tup_Int_Int_1 x55) 
    (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(synth-fun f$1 ((x56 Int) (x57 synd_tup_Int_Int)) Int ((Ix Int) (Ic Int))
 ((Ix Int
   (Ic x56 (proj_synd_tup_Int_Int_0 x57) (proj_synd_tup_Int_Int_1 x57) 
    (- Ix) (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i2 Int)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint (= 0 (f$0 i0 (mk_synd_tup_Int_Int 0 0))))
(constraint
 (= i0 (f$0 i0 (mk_synd_tup_Int_Int (+ (* i1 0) 0) (f$1 i1 (mk_synd_tup_Int_Int 0 0))))))
(constraint
 (= (+ (* i0 2) i1)
  (f$0 i0
   (mk_synd_tup_Int_Int (+ (* i1 (+ 1 0)) (+ (* i2 0) 0))
    (f$1 i1
     (mk_synd_tup_Int_Int (f$0 i2 (mk_synd_tup_Int_Int 0 0)) (f$1 i2 (mk_synd_tup_Int_Int 0 0))))))))
(check-synth)
