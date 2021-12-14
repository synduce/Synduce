(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x10 synd_tup_Int_Int) (x11 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x10) (proj_synd_tup_Int_Int_1 x10) (proj_synd_tup_Int_Int_0 x11)
    (proj_synd_tup_Int_Int_1 x11) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i0 Int)
(declare-var i236 Int)
(declare-var i235 Int)
(constraint
 (or (not (>= i235 (- i236)))
  (= i236 (odot$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i235 i236)))))
(constraint
 (or (not (>= i235 (- i236)))
  (= (max i236 (min i0 i235))
   (odot$1 (mk_synd_tup_Int_Int (max i0 0) (max 0 (min i0 0))) (mk_synd_tup_Int_Int i235 i236)))))
(check-synth)
