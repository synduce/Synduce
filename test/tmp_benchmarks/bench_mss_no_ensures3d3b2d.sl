(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$1 ((x205 synd_tup_Int_Int_Int_Int) (x206 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x205) (proj_synd_tup_Int_Int_Int_Int_1 x205)
    (proj_synd_tup_Int_Int_Int_Int_2 x205) (proj_synd_tup_Int_Int_Int_Int_3 x205)
    (proj_synd_tup_Int_Int_Int_Int_0 x206) (proj_synd_tup_Int_Int_Int_Int_1 x206)
    (proj_synd_tup_Int_Int_Int_Int_2 x206) (proj_synd_tup_Int_Int_Int_Int_3 x206) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i1069 Int)
(declare-var i1044 Int)
(declare-var i1009 Int)
(declare-var i978 Int)
(declare-var i972 Int)
(declare-var p4 Int)
(declare-var i1077 Int)
(declare-var i1076 Int)
(declare-var i1075 Int)
(declare-var i1074 Int)
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (and (and (>= i1076 i1074) (>= i1077 i1074)) (>= i1077 i1075)) (>= i1077 i1076))
       (>= i1075 0))
      (>= i1075 (- i1076)))
     (>= i1076 0))
    (>= i1075 i1074)))
  (= i1075
   (odot$1 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0)
    (mk_synd_tup_Int_Int_Int_Int i1074 i1075 i1076 i1077)))))
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (and (and (>= i1076 i1074) (>= i1077 i1074)) (>= i1077 i1075)) (>= i1077 i1076))
       (>= i1075 0))
      (>= i1075 (- i1076)))
     (>= i1076 0))
    (>= i1075 i1074)))
  (= (max i1075 (+ i1074 p4))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
     (max 0 (max (+ 0 p4) 0)))
    (mk_synd_tup_Int_Int_Int_Int i1074 i1075 i1076 i1077)))))
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (and (and (>= i1076 i1074) (>= i1077 i1074)) (>= i1077 i1075)) (>= i1077 i1076))
       (>= i1075 0))
      (>= i1075 (- i1076)))
     (>= i1076 0))
    (>= i1075 i1074)))
  (= (max i1075 (+ i1074 i972))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ 0 i972) (max 0 (+ 0 i972)) (max (+ 0 i972) 0)
     (max 0 (max (+ 0 i972) 0)))
    (mk_synd_tup_Int_Int_Int_Int i1074 i1075 i1076 i1077)))))
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (and (and (>= i1076 i1074) (>= i1077 i1074)) (>= i1077 i1075)) (>= i1077 i1076))
       (>= i1075 0))
      (>= i1075 (- i1076)))
     (>= i1076 0))
    (>= i1075 i1074)))
  (= (max (max i1075 (+ i1074 i978)) (+ (+ i1074 i978) i972))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ (+ 0 i978) i972) (max (max 0 (+ 0 i978)) (+ (+ 0 i978) i972))
     (max (+ (max (+ 0 i978) 0) i972) 0)
     (max (max 0 (max (+ 0 i978) 0)) (max (+ (max (+ 0 i978) 0) i972) 0)))
    (mk_synd_tup_Int_Int_Int_Int i1074 i1075 i1076 i1077)))))
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (and (and (>= i1076 i1074) (>= i1077 i1074)) (>= i1077 i1075)) (>= i1077 i1076))
       (>= i1075 0))
      (>= i1075 (- i1076)))
     (>= i1076 0))
    (>= i1075 i1074)))
  (= (max (max i1075 (+ i1074 i1044)) (+ (+ i1074 i1044) i1009))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ (+ 0 i1044) i1009)
     (max (max 0 (+ 0 i1044)) (+ (+ 0 i1044) i1009)) (max (+ (max (+ 0 i1044) 0) i1009) 0)
     (max (max 0 (max (+ 0 i1044) 0)) (max (+ (max (+ 0 i1044) 0) i1009) 0)))
    (mk_synd_tup_Int_Int_Int_Int i1074 i1075 i1076 i1077)))))
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (and (and (>= i1076 i1074) (>= i1077 i1074)) (>= i1077 i1075)) (>= i1077 i1076))
       (>= i1075 0))
      (>= i1075 (- i1076)))
     (>= i1076 0))
    (>= i1075 i1074)))
  (=
   (max (max (max i1075 (+ i1074 i1069)) (+ (+ i1074 i1069) i1044))
    (+ (+ (+ i1074 i1069) i1044) i1009))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ (+ (+ 0 i1069) i1044) i1009)
     (max (max (max 0 (+ 0 i1069)) (+ (+ 0 i1069) i1044)) (+ (+ (+ 0 i1069) i1044) i1009))
     (max (+ (max (+ (max (+ 0 i1069) 0) i1044) 0) i1009) 0)
     (max (max (max 0 (max (+ 0 i1069) 0)) (max (+ (max (+ 0 i1069) 0) i1044) 0))
      (max (+ (max (+ (max (+ 0 i1069) 0) i1044) 0) i1009) 0)))
    (mk_synd_tup_Int_Int_Int_Int i1074 i1075 i1076 i1077)))))
(check-synth)
