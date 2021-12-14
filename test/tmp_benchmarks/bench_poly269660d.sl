(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun join$0 ((x24 Int) (x25 Int) (x26 synd_tup_Int_Int) (x27 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((+ Ix Ix)))
  (Ix Int
   (Ic x24 x25 (proj_synd_tup_Int_Int_0 x26) (proj_synd_tup_Int_Int_1 x26)
    (proj_synd_tup_Int_Int_0 x27) (proj_synd_tup_Int_Int_1 x27) (- Ix) 
    (+ Ix Ix) (* Ix Ix) (div Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var p2 Int)
(declare-var i8 Int)
(declare-var i7 Int)
(declare-var p Int)
(declare-var x Int)
(constraint (= (+ p (* x i7)) (join$0 x p (mk_synd_tup_Int_Int i7 i8) (mk_synd_tup_Int_Int 0 1))))
(constraint
 (= (+ p2 (* x (+ p (* x i7))))
  (join$0 x p (mk_synd_tup_Int_Int i7 i8) (mk_synd_tup_Int_Int (+ p2 (* x 0)) (* x 1)))))
(check-synth)
