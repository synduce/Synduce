(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$2 ((x53 synd_tup_Int_Int_Int_Int) (x54 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x53) (proj_synd_tup_Int_Int_Int_Int_1 x53)
    (proj_synd_tup_Int_Int_Int_Int_2 x53) (proj_synd_tup_Int_Int_Int_Int_3 x53)
    (proj_synd_tup_Int_Int_Int_Int_0 x54) (proj_synd_tup_Int_Int_Int_Int_1 x54)
    (proj_synd_tup_Int_Int_Int_Int_2 x54) (proj_synd_tup_Int_Int_Int_Int_3 x54) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i26 Int)
(declare-var i14 Int)
(declare-var i534 Int)
(declare-var i533 Int)
(declare-var i532 Int)
(declare-var i531 Int)
(declare-var i2 Int)
(constraint
 (or (not (>= i533 i534))
  (= (max (+ i534 i2) i533)
   (s1$2 (mk_synd_tup_Int_Int_Int_Int i531 i532 i533 i534)
    (mk_synd_tup_Int_Int_Int_Int (max i2 0) (max i2 0) (max i2 0) i2)))))
(constraint
 (or (not (>= i533 i534))
  (= (max (+ (+ i534 i26) i14) (max (+ i534 i26) i533))
   (s1$2 (mk_synd_tup_Int_Int_Int_Int i531 i532 i533 i534)
    (mk_synd_tup_Int_Int_Int_Int (max (+ (max i26 0) i14) 0)
     (max (max i26 0) (max (+ (max i26 0) i14) 0)) (max (+ i26 i14) (max i26 0)) 
     (+ i26 i14))))))
(check-synth)
