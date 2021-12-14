(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x56 synd_tup_Int_Int) (x57 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x56) (proj_synd_tup_Int_Int_1 x56) (proj_synd_tup_Int_Int_0 x57)
    (proj_synd_tup_Int_Int_1 x57) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i707 Int)
(declare-var i697 Int)
(declare-var i0 Int)
(declare-var i1097 Int)
(declare-var i1096 Int)
(constraint
 (or
  (not
   (and
    (and (and (>= (+ i1096 i1097) (max i1096 i1097)) (>= (+ i1096 i1097) (max i1096 i1097)))
     (>= i1097 0))
    (>= i1096 (- i1097))))
  (= i1097 (odot$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i1096 i1097)))))
(constraint
 (or
  (not
   (and
    (and (and (>= (+ i1096 i1097) (max i1096 i1097)) (>= (+ i1096 i1097) (max i1096 i1097)))
     (>= i1097 0))
    (>= i1096 (- i1097))))
  (= (max i1097 (min i0 i1096))
   (odot$1 (mk_synd_tup_Int_Int (max i0 0) (max 0 (min i0 0))) (mk_synd_tup_Int_Int i1096 i1097)))))
(constraint
 (or (not (>= i1096 0))
  (= (max i1097 (min i697 i1096))
   (odot$1 (mk_synd_tup_Int_Int (max i697 0) (max 0 (min i697 0)))
    (mk_synd_tup_Int_Int i1096 i1097)))))
(constraint
 (or (not (>= i1096 0))
  (= (max (max i1097 (min i707 i1096)) (min i697 (max i707 i1096)))
   (odot$1
    (mk_synd_tup_Int_Int (max i697 (max i707 0))
     (max (max 0 (min i707 0)) (min i697 (max i707 0))))
    (mk_synd_tup_Int_Int i1096 i1097)))))
(check-synth)
