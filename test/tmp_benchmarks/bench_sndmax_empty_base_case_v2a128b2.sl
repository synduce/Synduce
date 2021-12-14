(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x20 synd_tup_Int_Int) (x21 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x20) (proj_synd_tup_Int_Int_1 x20) (proj_synd_tup_Int_Int_0 x21)
    (proj_synd_tup_Int_Int_1 x21) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i0 Int)
(declare-var i455 Int)
(declare-var i454 Int)
(constraint
 (or (not (and (>= i455 0) (>= i454 (- i455))))
  (= i455 (odot$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i454 i455)))))
(constraint
 (or (not (and (>= i455 0) (>= i454 (- i455))))
  (= (max i455 (min i0 i454))
   (odot$1 (mk_synd_tup_Int_Int (max i0 0) (max 0 (min i0 0))) (mk_synd_tup_Int_Int i454 i455)))))
(check-synth)
