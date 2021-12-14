(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$2 ((x157 synd_tup_Int_Int_Int_Int) (x158 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x157) (proj_synd_tup_Int_Int_Int_Int_1 x157)
    (proj_synd_tup_Int_Int_Int_Int_2 x157) (proj_synd_tup_Int_Int_Int_Int_3 x157)
    (proj_synd_tup_Int_Int_Int_Int_0 x158) (proj_synd_tup_Int_Int_Int_Int_1 x158)
    (proj_synd_tup_Int_Int_Int_Int_2 x158) (proj_synd_tup_Int_Int_Int_Int_3 x158) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i26 Int)
(declare-var i14 Int)
(declare-var i5534 Int)
(declare-var i5533 Int)
(declare-var i5532 Int)
(declare-var i5531 Int)
(declare-var i2 Int)
(constraint
 (or
  (not
   (and (and (and (and (>= i5533 i5534) (>= i5532 i5533)) (>= i5532 i5531)) (>= i5531 i5534))
    (>= i5531 0)))
  (= (max (+ i5534 i2) i5533)
   (s1$2 (mk_synd_tup_Int_Int_Int_Int i5531 i5532 i5533 i5534)
    (mk_synd_tup_Int_Int_Int_Int (max i2 0) (max i2 0) (max i2 0) i2)))))
(constraint
 (or
  (not
   (and (and (and (and (>= i5533 i5534) (>= i5532 i5533)) (>= i5532 i5531)) (>= i5531 i5534))
    (>= i5531 0)))
  (= (max (+ (+ i5534 i26) i14) (max (+ i5534 i26) i5533))
   (s1$2 (mk_synd_tup_Int_Int_Int_Int i5531 i5532 i5533 i5534)
    (mk_synd_tup_Int_Int_Int_Int (max (+ (max i26 0) i14) 0)
     (max (max i26 0) (max (+ (max i26 0) i14) 0)) (max (+ i26 i14) (max i26 0)) 
     (+ i26 i14))))))
(check-synth)
