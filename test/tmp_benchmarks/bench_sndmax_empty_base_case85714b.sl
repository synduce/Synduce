(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x12 synd_tup_Int_Int) (x13 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x12) (proj_synd_tup_Int_Int_1 x12) (proj_synd_tup_Int_Int_0 x13)
    (proj_synd_tup_Int_Int_1 x13) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i0 Int)
(declare-var i236 Int)
(declare-var i235 Int)
(constraint
 (or (not (and (>= i235 i236) (>= i236 0)))
  (= i236 (odot$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i235 i236)))))
(constraint
 (or (not (and (>= i235 i236) (and (= i0 i0) (>= i236 0))))
  (= (max i236 (min i0 i235))
   (odot$1 (mk_synd_tup_Int_Int (max i0 0) (max 0 (min i0 0))) (mk_synd_tup_Int_Int i235 i236)))))
(check-synth)
