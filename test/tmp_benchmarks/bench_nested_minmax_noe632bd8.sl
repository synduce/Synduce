(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun s1$0 ((x32 synd_tup_Int_Int) (x33 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((min Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x32) (proj_synd_tup_Int_Int_1 x32) (proj_synd_tup_Int_Int_0 x33)
    (proj_synd_tup_Int_Int_1 x33) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i12 Int)
(declare-var i1 Int)
(declare-var i16 Int)
(declare-var i15 Int)
(declare-var i2 Int)
(constraint (= (min i2 i15) (s1$0 (mk_synd_tup_Int_Int i15 i16) (mk_synd_tup_Int_Int i2 i2))))
(constraint
 (= (min (min i12 i1) i15)
  (s1$0 (mk_synd_tup_Int_Int i15 i16) (mk_synd_tup_Int_Int (min i12 i1) (max i12 i1)))))
(check-synth)
