(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$1 ((x131 synd_tup_Int_Int_Int_Int) (x132 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x131) (proj_synd_tup_Int_Int_Int_Int_1 x131)
    (proj_synd_tup_Int_Int_Int_Int_2 x131) (proj_synd_tup_Int_Int_Int_Int_3 x131)
    (proj_synd_tup_Int_Int_Int_Int_0 x132) (proj_synd_tup_Int_Int_Int_Int_1 x132)
    (proj_synd_tup_Int_Int_Int_Int_2 x132) (proj_synd_tup_Int_Int_Int_Int_3 x132) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p4 Int)
(declare-var i915 Int)
(declare-var i914 Int)
(declare-var i913 Int)
(declare-var i912 Int)
(constraint
 (or
  (not
   (and
    (and
     (and (and (and (and (>= i914 i912) (>= i915 i912)) (>= i915 i913)) (>= i915 i914))
      (>= i913 0))
     (>= i913 (- i914)))
    (>= i914 0)))
  (= i913
   (odot$1 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0) (mk_synd_tup_Int_Int_Int_Int i912 i913 i914 i915)))))
(constraint
 (or
  (not
   (and
    (and
     (and (and (and (and (>= i914 i912) (>= i915 i912)) (>= i915 i913)) (>= i915 i914))
      (>= i913 0))
     (>= i913 (- i914)))
    (>= i914 0)))
  (= (max i913 (+ i912 p4))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
     (max 0 (max (+ 0 p4) 0)))
    (mk_synd_tup_Int_Int_Int_Int i912 i913 i914 i915)))))
(check-synth)
