(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$3 ((x67 synd_tup_Int_Int_Int_Int) (x68 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x67) (proj_synd_tup_Int_Int_Int_Int_1 x67)
    (proj_synd_tup_Int_Int_Int_Int_2 x67) (proj_synd_tup_Int_Int_Int_Int_3 x67)
    (proj_synd_tup_Int_Int_Int_Int_0 x68) (proj_synd_tup_Int_Int_Int_Int_1 x68)
    (proj_synd_tup_Int_Int_Int_Int_2 x68) (proj_synd_tup_Int_Int_Int_Int_3 x68) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p4 Int)
(declare-var i540 Int)
(declare-var i539 Int)
(declare-var i538 Int)
(declare-var i537 Int)
(constraint
 (or (not (and (and (and (>= i539 i537) (>= i540 i537)) (>= i540 i538)) (>= i540 i539)))
  (= i540
   (odot$3 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0) (mk_synd_tup_Int_Int_Int_Int i537 i538 i539 i540)))))
(constraint
 (or (not (and (and (and (>= i539 i537) (>= i540 i537)) (>= i540 i538)) (>= i540 i539)))
  (= (max i540 (max (+ i539 p4) 0))
   (odot$3
    (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
     (max 0 (max (+ 0 p4) 0)))
    (mk_synd_tup_Int_Int_Int_Int i537 i538 i539 i540)))))
(check-synth)
