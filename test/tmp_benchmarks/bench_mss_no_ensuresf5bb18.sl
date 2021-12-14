(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$3 ((x53 synd_tup_Int_Int_Int_Int) (x54 synd_tup_Int_Int_Int_Int)) Int
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
(declare-var p4 Int)
(declare-var i494 Int)
(declare-var i493 Int)
(declare-var i492 Int)
(declare-var i491 Int)
(constraint
 (or (not (and (and (>= i493 i491) (>= i494 i491)) (>= i494 i492)))
  (= i494
   (odot$3 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0) (mk_synd_tup_Int_Int_Int_Int i491 i492 i493 i494)))))
(constraint
 (or (not (and (and (>= i493 i491) (>= i494 i491)) (>= i494 i492)))
  (= (max i494 (max (+ i493 p4) 0))
   (odot$3
    (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
     (max 0 (max (+ 0 p4) 0)))
    (mk_synd_tup_Int_Int_Int_Int i491 i492 i493 i494)))))
(check-synth)
