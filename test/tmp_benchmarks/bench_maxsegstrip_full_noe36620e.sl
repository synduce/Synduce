(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$1 ((x87 synd_tup_Int_Int_Int_Int) (x88 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x87) (proj_synd_tup_Int_Int_Int_Int_1 x87)
    (proj_synd_tup_Int_Int_Int_Int_2 x87) (proj_synd_tup_Int_Int_Int_Int_3 x87)
    (proj_synd_tup_Int_Int_Int_Int_0 x88) (proj_synd_tup_Int_Int_Int_Int_1 x88)
    (proj_synd_tup_Int_Int_Int_Int_2 x88) (proj_synd_tup_Int_Int_Int_Int_3 x88) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i34 Int)
(declare-var i22 Int)
(declare-var i498 Int)
(declare-var i497 Int)
(declare-var i496 Int)
(declare-var i495 Int)
(declare-var i2 Int)
(constraint
 (or (not (>= i497 i498))
  (= (max i496 (max (+ i495 i2) 0))
   (s1$1 (mk_synd_tup_Int_Int_Int_Int i495 i496 i497 i498)
    (mk_synd_tup_Int_Int_Int_Int (max i2 0) (max i2 0) (max i2 0) i2)))))
(constraint
 (or (not (>= i497 i498))
  (= (max (max i496 (max (+ i495 i34) 0)) (max (+ (max (+ i495 i34) 0) i22) 0))
   (s1$1 (mk_synd_tup_Int_Int_Int_Int i495 i496 i497 i498)
    (mk_synd_tup_Int_Int_Int_Int (max (+ (max i34 0) i22) 0)
     (max (max i34 0) (max (+ (max i34 0) i22) 0)) (max (+ i34 i22) (max i34 0)) 
     (+ i34 i22))))))
(check-synth)
