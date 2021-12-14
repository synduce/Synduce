(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$0 ((x56 synd_tup_Int_Int) (x57 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x56) (proj_synd_tup_Int_Int_1 x56) (proj_synd_tup_Int_Int_0 x57)
    (proj_synd_tup_Int_Int_1 x57) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i251 Int)
(declare-var i241 Int)
(declare-var i0 Int)
(declare-var i1222 Int)
(declare-var i1221 Int)
(constraint
 (or
  (not
   (and (>= i1221 i1222)
    (and (and (and (>= i1222 0) (>= i1221 (- i1222))) (>= i1222 0)) (>= i1222 0))))
  (= i1221 (odot$0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i1221 i1222)))))
(constraint
 (or
  (not
   (and (>= i1221 i1222)
    (and
     (and (and (and (= i0 i0) (>= i1222 0)) (and (= i0 i0) (>= i1222 0)))
      (and (= i0 i0) (>= i1221 (- i1222))))
     (and (= i0 i0) (>= i1222 0)))))
  (= (max i0 i1221)
   (odot$0 (mk_synd_tup_Int_Int (max i0 0) (max 0 (min i0 0))) (mk_synd_tup_Int_Int i1221 i1222)))))
(constraint
 (or
  (not
   (and (>= i1221 i1222)
    (and (and (and (= i241 i241) (>= i1222 0)) (and (= i241 i241) (>= i1222 0)))
     (and (= i241 i241) (>= i1221 0)))))
  (= (max i241 i1221)
   (odot$0 (mk_synd_tup_Int_Int (max i241 0) (max 0 (min i241 0)))
    (mk_synd_tup_Int_Int i1221 i1222)))))
(constraint
 (or
  (not
   (and (>= i1221 i1222)
    (and
     (and (and (>= i241 i241) (and (= i241 i241) (>= i1222 0)))
      (and (>= i241 i241) (and (>= i241 i241) (>= i1221 (- i1222)))))
     (and (>= i241 i241) (and (= i241 i241) (>= i1221 0))))))
  (= (max i241 (max i251 i1221))
   (odot$0
    (mk_synd_tup_Int_Int (max i241 (max i251 0))
     (max (max 0 (min i251 0)) (min i241 (max i251 0))))
    (mk_synd_tup_Int_Int i1221 i1222)))))
(check-synth)
