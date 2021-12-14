(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$3 ((x23 synd_tup_Int_Int_Int_Int) (x24 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x23) (proj_synd_tup_Int_Int_Int_Int_1 x23)
    (proj_synd_tup_Int_Int_Int_Int_2 x23) (proj_synd_tup_Int_Int_Int_Int_3 x23)
    (proj_synd_tup_Int_Int_Int_Int_0 x24) (proj_synd_tup_Int_Int_Int_Int_1 x24)
    (proj_synd_tup_Int_Int_Int_Int_2 x24) (proj_synd_tup_Int_Int_Int_Int_3 x24) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p4 Int)
(declare-var i165 Int)
(declare-var i164 Int)
(declare-var i163 Int)
(declare-var i162 Int)
(constraint
 (or (not (>= i164 i162))
  (= i165
   (odot$3 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0) (mk_synd_tup_Int_Int_Int_Int i162 i163 i164 i165)))))
(constraint
 (or (not (>= i164 i162))
  (= (max i165 (max (+ i164 p4) 0))
   (odot$3
    (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
     (max 0 (max (+ 0 p4) 0)))
    (mk_synd_tup_Int_Int_Int_Int i162 i163 i164 i165)))))
(check-synth)
