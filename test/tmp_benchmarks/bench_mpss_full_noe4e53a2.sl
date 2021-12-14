(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$0 ((x43 synd_tup_Int_Int) (x44 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x43) (proj_synd_tup_Int_Int_1 x43) (proj_synd_tup_Int_Int_0 x44)
    (proj_synd_tup_Int_Int_1 x44) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i24 Int)
(declare-var i14 Int)
(declare-var i28 Int)
(declare-var i27 Int)
(declare-var i2 Int)
(constraint
 (= (max (+ i28 i2) i27) (s1$0 (mk_synd_tup_Int_Int i27 i28) (mk_synd_tup_Int_Int (max 0 i2) i2))))
(constraint
 (= (max (+ (+ i28 i24) i14) (max (+ i28 i24) i27))
  (s1$0 (mk_synd_tup_Int_Int i27 i28)
   (mk_synd_tup_Int_Int (max (+ i24 i14) (max 0 i24)) (+ i24 i14)))))
(check-synth)
