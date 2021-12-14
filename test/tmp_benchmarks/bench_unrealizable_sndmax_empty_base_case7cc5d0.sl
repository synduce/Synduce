(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x36 synd_tup_Int_Int) (x37 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x36) (proj_synd_tup_Int_Int_1 x36) (proj_synd_tup_Int_Int_0 x37)
    (proj_synd_tup_Int_Int_1 x37) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i251 Int)
(declare-var i241 Int)
(declare-var i0 Int)
(declare-var i641 Int)
(declare-var i640 Int)
(constraint
 (or (not (and (>= i640 i641) (and (>= i641 0) (>= i641 0))))
  (= i641 (odot$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i640 i641)))))
(constraint
 (or
  (not (and (>= i640 i641) (and (and (= i0 i0) (>= i640 (- i641))) (and (= i0 i0) (>= i641 0)))))
  (= (max i641 (min i0 i640))
   (odot$1 (mk_synd_tup_Int_Int (max i0 0) (max 0 (min i0 0))) (mk_synd_tup_Int_Int i640 i641)))))
(constraint
 (or (not (and (>= i640 i641) (and (= i241 i241) (>= i640 0))))
  (= (max i641 (min i241 i640))
   (odot$1 (mk_synd_tup_Int_Int (max i241 0) (max 0 (min i241 0))) (mk_synd_tup_Int_Int i640 i641)))))
(constraint
 (or (not (and (>= i640 i641) (and (>= i241 i241) (and (= i241 i241) (>= i640 0)))))
  (= (max (max i641 (min i251 i640)) (min i241 (max i251 i640)))
   (odot$1
    (mk_synd_tup_Int_Int (max i241 (max i251 0))
     (max (max 0 (min i251 0)) (min i241 (max i251 0))))
    (mk_synd_tup_Int_Int i640 i641)))))
(check-synth)
