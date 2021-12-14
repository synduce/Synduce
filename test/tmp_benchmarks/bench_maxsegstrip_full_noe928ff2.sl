(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$1 ((x189 synd_tup_Int_Int_Int_Int) (x190 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x189) (proj_synd_tup_Int_Int_Int_Int_1 x189)
    (proj_synd_tup_Int_Int_Int_Int_2 x189) (proj_synd_tup_Int_Int_Int_Int_3 x189)
    (proj_synd_tup_Int_Int_Int_Int_0 x190) (proj_synd_tup_Int_Int_Int_Int_1 x190)
    (proj_synd_tup_Int_Int_Int_Int_2 x190) (proj_synd_tup_Int_Int_Int_Int_3 x190) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i34 Int)
(declare-var i22 Int)
(declare-var i5430 Int)
(declare-var i5429 Int)
(declare-var i5428 Int)
(declare-var i5427 Int)
(declare-var i2 Int)
(constraint
 (or
  (not
   (and (and (and (and (>= i5429 i5430) (>= i5428 i5429)) (>= i5428 i5427)) (>= i5427 i5430))
    (>= i5427 0)))
  (= (max i5428 (max (+ i5427 i2) 0))
   (s1$1 (mk_synd_tup_Int_Int_Int_Int i5427 i5428 i5429 i5430)
    (mk_synd_tup_Int_Int_Int_Int (max i2 0) (max i2 0) (max i2 0) i2)))))
(constraint
 (or
  (not
   (and (and (and (and (>= i5429 i5430) (>= i5428 i5429)) (>= i5428 i5427)) (>= i5427 i5430))
    (>= i5427 0)))
  (= (max (max i5428 (max (+ i5427 i34) 0)) (max (+ (max (+ i5427 i34) 0) i22) 0))
   (s1$1 (mk_synd_tup_Int_Int_Int_Int i5427 i5428 i5429 i5430)
    (mk_synd_tup_Int_Int_Int_Int (max (+ (max i34 0) i22) 0)
     (max (max i34 0) (max (+ (max i34 0) i22) 0)) (max (+ i34 i22) (max i34 0)) 
     (+ i34 i22))))))
(check-synth)
