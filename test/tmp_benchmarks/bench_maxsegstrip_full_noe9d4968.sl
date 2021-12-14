(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$2 ((x108 synd_tup_Int_Int_Int_Int) (x109 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x108) (proj_synd_tup_Int_Int_Int_Int_1 x108)
    (proj_synd_tup_Int_Int_Int_Int_2 x108) (proj_synd_tup_Int_Int_Int_Int_3 x108)
    (proj_synd_tup_Int_Int_Int_Int_0 x109) (proj_synd_tup_Int_Int_Int_Int_1 x109)
    (proj_synd_tup_Int_Int_Int_Int_2 x109) (proj_synd_tup_Int_Int_Int_Int_3 x109) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i34 Int)
(declare-var i22 Int)
(declare-var i2378 Int)
(declare-var i2377 Int)
(declare-var i2376 Int)
(declare-var i2375 Int)
(declare-var i2 Int)
(constraint
 (or (not (and (>= i2377 i2378) (>= i2376 i2377)))
  (= (max (+ i2378 i2) i2377)
   (s1$2 (mk_synd_tup_Int_Int_Int_Int i2375 i2376 i2377 i2378)
    (mk_synd_tup_Int_Int_Int_Int (max i2 0) (max i2 0) (max i2 0) i2)))))
(constraint
 (or (not (and (>= i2377 i2378) (>= i2376 i2377)))
  (= (max (+ (+ i2378 i34) i22) (max (+ i2378 i34) i2377))
   (s1$2 (mk_synd_tup_Int_Int_Int_Int i2375 i2376 i2377 i2378)
    (mk_synd_tup_Int_Int_Int_Int (max (+ (max i34 0) i22) 0)
     (max (max i34 0) (max (+ (max i34 0) i22) 0)) (max (+ i34 i22) (max i34 0)) 
     (+ i34 i22))))))
(check-synth)
