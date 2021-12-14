(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$1 ((x85 synd_tup_Int_Int_Int_Int) (x86 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x85) (proj_synd_tup_Int_Int_Int_Int_1 x85)
    (proj_synd_tup_Int_Int_Int_Int_2 x85) (proj_synd_tup_Int_Int_Int_Int_3 x85)
    (proj_synd_tup_Int_Int_Int_Int_0 x86) (proj_synd_tup_Int_Int_Int_Int_1 x86)
    (proj_synd_tup_Int_Int_Int_Int_2 x86) (proj_synd_tup_Int_Int_Int_Int_3 x86) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i26 Int)
(declare-var i14 Int)
(declare-var i2444 Int)
(declare-var i2443 Int)
(declare-var i2442 Int)
(declare-var i2441 Int)
(declare-var i2 Int)
(constraint
 (or (not (and (>= i2443 i2444) (>= i2442 i2443)))
  (= (max i2442 (max (+ i2441 i2) 0))
   (s1$1 (mk_synd_tup_Int_Int_Int_Int i2441 i2442 i2443 i2444)
    (mk_synd_tup_Int_Int_Int_Int (max i2 0) (max i2 0) (max i2 0) i2)))))
(constraint
 (or (not (and (>= i2443 i2444) (>= i2442 i2443)))
  (= (max (max i2442 (max (+ i2441 i26) 0)) (max (+ (max (+ i2441 i26) 0) i14) 0))
   (s1$1 (mk_synd_tup_Int_Int_Int_Int i2441 i2442 i2443 i2444)
    (mk_synd_tup_Int_Int_Int_Int (max (+ (max i26 0) i14) 0)
     (max (max i26 0) (max (+ (max i26 0) i14) 0)) (max (+ i26 i14) (max i26 0)) 
     (+ i26 i14))))))
(check-synth)
