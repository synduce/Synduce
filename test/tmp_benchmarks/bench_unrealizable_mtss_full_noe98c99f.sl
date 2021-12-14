(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$0 ((x45 synd_tup_Int_Int) (x46 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x45) (proj_synd_tup_Int_Int_1 x45) (proj_synd_tup_Int_Int_0 x46)
    (proj_synd_tup_Int_Int_1 x46) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i24 Int)
(declare-var i14 Int)
(declare-var i28 Int)
(declare-var i27 Int)
(declare-var i2 Int)
(constraint
 (= (max (+ i27 i2) 0) (s1$0 (mk_synd_tup_Int_Int i27 i28) (mk_synd_tup_Int_Int (max 0 i2) i2))))
(constraint
 (= (max (+ (max (+ i27 i24) 0) i14) 0)
  (s1$0 (mk_synd_tup_Int_Int i27 i28)
   (mk_synd_tup_Int_Int (max (+ (max 0 i24) i14) 0) (+ i24 i14)))))
(check-synth)
