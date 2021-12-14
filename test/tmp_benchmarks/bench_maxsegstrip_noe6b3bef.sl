(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$2 ((x34 synd_tup_Int_Int_Int_Int) (x35 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x34) (proj_synd_tup_Int_Int_Int_Int_1 x34)
    (proj_synd_tup_Int_Int_Int_Int_2 x34) (proj_synd_tup_Int_Int_Int_Int_3 x34)
    (proj_synd_tup_Int_Int_Int_Int_0 x35) (proj_synd_tup_Int_Int_Int_Int_1 x35)
    (proj_synd_tup_Int_Int_Int_Int_2 x35) (proj_synd_tup_Int_Int_Int_Int_3 x35) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i26 Int)
(declare-var i14 Int)
(declare-var i34 Int)
(declare-var i33 Int)
(declare-var i32 Int)
(declare-var i31 Int)
(declare-var i2 Int)
(constraint
 (= (max (+ i34 i2) i33)
  (s1$2 (mk_synd_tup_Int_Int_Int_Int i31 i32 i33 i34)
   (mk_synd_tup_Int_Int_Int_Int (max i2 0) (max i2 0) (max i2 0) i2))))
(constraint
 (= (max (+ (+ i34 i26) i14) (max (+ i34 i26) i33))
  (s1$2 (mk_synd_tup_Int_Int_Int_Int i31 i32 i33 i34)
   (mk_synd_tup_Int_Int_Int_Int (max (+ (max i26 0) i14) 0)
    (max (max i26 0) (max (+ (max i26 0) i14) 0)) (max (+ i26 i14) (max i26 0)) 
    (+ i26 i14)))))
(check-synth)
