(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join$0 ((x10 synd_tup_Int_Int) (x11 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x10) (proj_synd_tup_Int_Int_1 x10) (proj_synd_tup_Int_Int_0 x11)
    (proj_synd_tup_Int_Int_1 x11) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i Int)
(declare-var i1 Int)
(declare-var i0 Int)
(constraint (= i0 (join$0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i0 i1))))
(constraint
 (= (max i0 (+ i1 i))
  (join$0 (mk_synd_tup_Int_Int (max 0 (+ 0 i)) (+ 0 i)) (mk_synd_tup_Int_Int i0 i1))))
(check-synth)
