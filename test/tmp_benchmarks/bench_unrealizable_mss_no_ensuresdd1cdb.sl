(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$3 ((x8 synd_tup_Int_Int_Int_Int) (x9 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x8) (proj_synd_tup_Int_Int_Int_Int_1 x8)
    (proj_synd_tup_Int_Int_Int_Int_2 x8) (proj_synd_tup_Int_Int_Int_Int_3 x8)
    (proj_synd_tup_Int_Int_Int_Int_0 x9) (proj_synd_tup_Int_Int_Int_Int_1 x9)
    (proj_synd_tup_Int_Int_Int_Int_2 x9) (proj_synd_tup_Int_Int_Int_Int_3 x9) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p4 Int)
(declare-var i2 Int)
(declare-var i1 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (= i2 (odot$3 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0) (mk_synd_tup_Int_Int_Int_Int i i0 i1 i2))))
(constraint
 (= (max i2 (max (+ i1 p4) 0))
  (odot$3
   (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
    (max 0 (max (+ 0 p4) 0)))
   (mk_synd_tup_Int_Int_Int_Int i i0 i1 i2))))
(check-synth)
