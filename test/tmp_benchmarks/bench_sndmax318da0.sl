(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x16 synd_tup_Int_Int) (x17 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x16) (proj_synd_tup_Int_Int_1 x16) (proj_synd_tup_Int_Int_0 x17)
    (proj_synd_tup_Int_Int_1 x17) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i21 Int)
(declare-var i11 Int)
(declare-var i0 Int)
(declare-var i31 Int)
(declare-var i30 Int)
(constraint
 (or (not (and (>= i30 i31) (>= i31 0)))
  (= i31 (odot$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i30 i31)))))
(constraint
 (or (not (and (>= i30 i31) (>= i31 0)))
  (= (max i31 (min i0 i30))
   (odot$1 (mk_synd_tup_Int_Int (max i0 0) (max 0 (min i0 0))) (mk_synd_tup_Int_Int i30 i31)))))
(constraint
 (or (not (and (>= i30 i31) (>= i31 0)))
  (= (max i31 (min i11 i30))
   (odot$1 (mk_synd_tup_Int_Int (max i11 0) (max 0 (min i11 0))) (mk_synd_tup_Int_Int i30 i31)))))
(constraint
 (or (not (and (>= i30 i31) (>= i31 0)))
  (= (max (max i31 (min i21 i30)) (min i11 (max i21 i30)))
   (odot$1
    (mk_synd_tup_Int_Int (max i11 (max i21 0)) (max (max 0 (min i21 0)) (min i11 (max i21 0))))
    (mk_synd_tup_Int_Int i30 i31)))))
(check-synth)
