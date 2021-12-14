(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x24 synd_tup_Int_Int) (x25 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x24) (proj_synd_tup_Int_Int_1 x24) (proj_synd_tup_Int_Int_0 x25)
    (proj_synd_tup_Int_Int_1 x25) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i251 Int)
(declare-var i241 Int)
(declare-var i0 Int)
(declare-var i261 Int)
(declare-var i260 Int)
(constraint
 (or (not (and (>= i260 i261) (>= i261 0)))
  (= i261 (odot$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i260 i261)))))
(constraint
 (or (not (and (>= i260 i261) (and (= i0 i0) (>= i261 0))))
  (= (max i261 (min i0 i260))
   (odot$1 (mk_synd_tup_Int_Int (max i0 0) (max 0 (min i0 0))) (mk_synd_tup_Int_Int i260 i261)))))
(constraint
 (or (not (>= i260 i261))
  (= (max i261 (min i241 i260))
   (odot$1 (mk_synd_tup_Int_Int (max i241 0) (max 0 (min i241 0))) (mk_synd_tup_Int_Int i260 i261)))))
(constraint
 (or (not (>= i260 i261))
  (= (max (max i261 (min i251 i260)) (min i241 (max i251 i260)))
   (odot$1
    (mk_synd_tup_Int_Int (max i241 (max i251 0))
     (max (max 0 (min i251 0)) (min i241 (max i251 0))))
    (mk_synd_tup_Int_Int i260 i261)))))
(check-synth)
