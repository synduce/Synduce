(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$1 ((x14 synd_tup_Int_Int_Int_Int) (x15 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x14) (proj_synd_tup_Int_Int_Int_Int_1 x14)
    (proj_synd_tup_Int_Int_Int_Int_2 x14) (proj_synd_tup_Int_Int_Int_Int_3 x14)
    (proj_synd_tup_Int_Int_Int_Int_0 x15) (proj_synd_tup_Int_Int_Int_Int_1 x15)
    (proj_synd_tup_Int_Int_Int_Int_2 x15) (proj_synd_tup_Int_Int_Int_Int_3 x15) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p4 Int)
(declare-var i2 Int)
(declare-var i1 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and (>= i0 0)
    (and (>= i1 0)
     (and (>= i1 i) (and (>= i0 i) (and (>= i2 0) (and (>= i2 i0) (and (>= i2 i) (>= i2 i1)))))))))
  (= i0 (odot$1 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0) (mk_synd_tup_Int_Int_Int_Int i i0 i1 i2)))))
(constraint
 (or
  (not
   (and (>= i0 0)
    (and (>= i1 0)
     (and (>= i1 i) (and (>= i0 i) (and (>= i2 0) (and (>= i2 i0) (and (>= i2 i) (>= i2 i1)))))))))
  (= (max i0 (+ i p4))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
     (max 0 (max (+ 0 p4) 0)))
    (mk_synd_tup_Int_Int_Int_Int i i0 i1 i2)))))
(check-synth)
