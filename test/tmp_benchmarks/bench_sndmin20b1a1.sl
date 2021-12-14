(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x2 synd_tup_Int_Int) (x3 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((min Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x2) (proj_synd_tup_Int_Int_1 x2) (proj_synd_tup_Int_Int_0 x3)
    (proj_synd_tup_Int_Int_1 x3) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i0 Int)
(declare-var i2 Int)
(declare-var i1 Int)
(constraint
 (or (not (<= i1 i2)) (= i2 (odot$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i1 i2)))))
(constraint
 (or (not (<= i1 i2))
  (= (min i2 (max i0 i1))
   (odot$1 (mk_synd_tup_Int_Int (min i0 0) (min 0 (max i0 0))) (mk_synd_tup_Int_Int i1 i2)))))
(check-synth)
