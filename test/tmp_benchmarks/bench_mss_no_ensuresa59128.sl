(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$1 ((x179 synd_tup_Int_Int_Int_Int) (x180 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x179) (proj_synd_tup_Int_Int_Int_Int_1 x179)
    (proj_synd_tup_Int_Int_Int_Int_2 x179) (proj_synd_tup_Int_Int_Int_Int_3 x179)
    (proj_synd_tup_Int_Int_Int_Int_0 x180) (proj_synd_tup_Int_Int_Int_Int_1 x180)
    (proj_synd_tup_Int_Int_Int_Int_2 x180) (proj_synd_tup_Int_Int_Int_Int_3 x180) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i978 Int)
(declare-var i972 Int)
(declare-var p4 Int)
(declare-var i986 Int)
(declare-var i985 Int)
(declare-var i984 Int)
(declare-var i983 Int)
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (and (and (>= i985 i983) (>= i986 i983)) (>= i986 i984)) (>= i986 i985))
       (>= i984 0))
      (>= i984 (- i985)))
     (>= i985 0))
    (>= i984 i983)))
  (= i984
   (odot$1 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0) (mk_synd_tup_Int_Int_Int_Int i983 i984 i985 i986)))))
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (and (and (>= i985 i983) (>= i986 i983)) (>= i986 i984)) (>= i986 i985))
       (>= i984 0))
      (>= i984 (- i985)))
     (>= i985 0))
    (>= i984 i983)))
  (= (max i984 (+ i983 p4))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
     (max 0 (max (+ 0 p4) 0)))
    (mk_synd_tup_Int_Int_Int_Int i983 i984 i985 i986)))))
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (and (and (>= i985 i983) (>= i986 i983)) (>= i986 i984)) (>= i986 i985))
       (>= i984 0))
      (>= i984 (- i985)))
     (>= i985 0))
    (>= i984 i983)))
  (= (max i984 (+ i983 i972))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ 0 i972) (max 0 (+ 0 i972)) (max (+ 0 i972) 0)
     (max 0 (max (+ 0 i972) 0)))
    (mk_synd_tup_Int_Int_Int_Int i983 i984 i985 i986)))))
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (and (and (>= i985 i983) (>= i986 i983)) (>= i986 i984)) (>= i986 i985))
       (>= i984 0))
      (>= i984 (- i985)))
     (>= i985 0))
    (>= i984 i983)))
  (= (max (max i984 (+ i983 i978)) (+ (+ i983 i978) i972))
   (odot$1
    (mk_synd_tup_Int_Int_Int_Int (+ (+ 0 i978) i972) (max (max 0 (+ 0 i978)) (+ (+ 0 i978) i972))
     (max (+ (max (+ 0 i978) 0) i972) 0)
     (max (max 0 (max (+ 0 i978) 0)) (max (+ (max (+ 0 i978) 0) i972) 0)))
    (mk_synd_tup_Int_Int_Int_Int i983 i984 i985 i986)))))
(check-synth)
