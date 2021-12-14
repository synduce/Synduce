(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x42 synd_tup_Int_Int) (x43 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x42) (proj_synd_tup_Int_Int_1 x42) (proj_synd_tup_Int_Int_0 x43)
    (proj_synd_tup_Int_Int_1 x43) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i707 Int)
(declare-var i697 Int)
(declare-var i0 Int)
(declare-var i717 Int)
(declare-var i716 Int)
(constraint
 (or (not (and (and (>= (+ i716 i717) (max i716 i717)) (>= i717 0)) (>= i716 (- i717))))
  (= i717 (odot$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i716 i717)))))
(constraint
 (or (not (and (and (>= (+ i716 i717) (max i716 i717)) (>= i717 0)) (>= i716 (- i717))))
  (= (max i717 (min i0 i716))
   (odot$1 (mk_synd_tup_Int_Int (max i0 0) (max 0 (min i0 0))) (mk_synd_tup_Int_Int i716 i717)))))
(constraint
 (= (max i717 (min i697 i716))
  (odot$1 (mk_synd_tup_Int_Int (max i697 0) (max 0 (min i697 0))) (mk_synd_tup_Int_Int i716 i717))))
(constraint
 (= (max (max i717 (min i707 i716)) (min i697 (max i707 i716)))
  (odot$1
   (mk_synd_tup_Int_Int (max i697 (max i707 0)) (max (max 0 (min i707 0)) (min i697 (max i707 0))))
   (mk_synd_tup_Int_Int i716 i717))))
(check-synth)
