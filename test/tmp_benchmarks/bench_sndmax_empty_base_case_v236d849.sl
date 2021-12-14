(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x32 synd_tup_Int_Int) (x33 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x32) (proj_synd_tup_Int_Int_1 x32) (proj_synd_tup_Int_Int_0 x33)
    (proj_synd_tup_Int_Int_1 x33) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i0 Int)
(declare-var i692 Int)
(declare-var i691 Int)
(constraint
 (or (not (and (and (>= (+ i691 i692) (max i691 i692)) (>= i692 0)) (>= i691 (- i692))))
  (= i692 (odot$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i691 i692)))))
(constraint
 (or (not (and (and (>= (+ i691 i692) (max i691 i692)) (>= i692 0)) (>= i691 (- i692))))
  (= (max i692 (min i0 i691))
   (odot$1 (mk_synd_tup_Int_Int (max i0 0) (max 0 (min i0 0))) (mk_synd_tup_Int_Int i691 i692)))))
(check-synth)
