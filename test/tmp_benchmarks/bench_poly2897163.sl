(set-logic DTNIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(synth-fun join$0 ((x28 Int) (x29 Int) (x30 synd_tup_Int_Int) (x31 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix)))
  (Ix Int
   (Ic x28 x29 (proj_synd_tup_Int_Int_0 x30) (proj_synd_tup_Int_Int_1 x30)
    (proj_synd_tup_Int_Int_0 x31) (proj_synd_tup_Int_Int_1 x31) (- Ix) 
    (+ Ix Ix) (* Ix Ix) (div Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
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
