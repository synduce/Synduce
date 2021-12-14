(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$1 ((x44 synd_tup_Int_Int) (x45 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x44) (proj_synd_tup_Int_Int_1 x44) (proj_synd_tup_Int_Int_0 x45)
    (proj_synd_tup_Int_Int_1 x45) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i251 Int)
(declare-var i241 Int)
(declare-var i0 Int)
(declare-var i1003 Int)
(declare-var i1002 Int)
(constraint
 (or (not (and (>= i1002 i1003) (and (and (>= i1002 (- i1003)) (>= i1003 0)) (>= i1003 0))))
  (= i1003 (odot$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i1002 i1003)))))
(constraint
 (or
  (not
   (and (>= i1002 i1003)
    (and (and (and (= i0 i0) (>= i1003 0)) (and (= i0 i0) (>= i1002 (- i1003))))
     (and (= i0 i0) (>= i1003 0)))))
  (= (max i1003 (min i0 i1002))
   (odot$1 (mk_synd_tup_Int_Int (max i0 0) (max 0 (min i0 0))) (mk_synd_tup_Int_Int i1002 i1003)))))
(constraint
 (or
  (not
   (and (>= i1002 i1003) (and (and (= i241 i241) (>= i1003 0)) (and (= i241 i241) (>= i1002 0)))))
  (= (max i1003 (min i241 i1002))
   (odot$1 (mk_synd_tup_Int_Int (max i241 0) (max 0 (min i241 0)))
    (mk_synd_tup_Int_Int i1002 i1003)))))
(constraint
 (or
  (not
   (and (>= i1002 i1003)
    (and (and (>= i241 i241) (and (>= i241 i241) (>= i1002 (- i1003))))
     (and (>= i241 i241) (and (= i241 i241) (>= i1002 0))))))
  (= (max (max i1003 (min i251 i1002)) (min i241 (max i251 i1002)))
   (odot$1
    (mk_synd_tup_Int_Int (max i241 (max i251 0))
     (max (max 0 (min i251 0)) (min i241 (max i251 0))))
    (mk_synd_tup_Int_Int i1002 i1003)))))
(check-synth)
