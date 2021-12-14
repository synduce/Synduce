(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$3 ((x85 synd_tup_Int_Int_Int_Int) (x86 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x85) (proj_synd_tup_Int_Int_Int_Int_1 x85)
    (proj_synd_tup_Int_Int_Int_Int_2 x85) (proj_synd_tup_Int_Int_Int_Int_3 x85)
    (proj_synd_tup_Int_Int_Int_Int_0 x86) (proj_synd_tup_Int_Int_Int_Int_1 x86)
    (proj_synd_tup_Int_Int_Int_Int_2 x86) (proj_synd_tup_Int_Int_Int_Int_3 x86) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p4 Int)
(declare-var i703 Int)
(declare-var i702 Int)
(declare-var i701 Int)
(declare-var i700 Int)
(constraint
 (or
  (not
   (and (and (and (and (>= i702 i700) (>= i703 i700)) (>= i703 i701)) (>= i703 i702)) (>= i701 0)))
  (= i703
   (odot$3 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0) (mk_synd_tup_Int_Int_Int_Int i700 i701 i702 i703)))))
(constraint
 (or
  (not
   (and (and (and (and (>= i702 i700) (>= i703 i700)) (>= i703 i701)) (>= i703 i702)) (>= i701 0)))
  (= (max i703 (max (+ i702 p4) 0))
   (odot$3
    (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
     (max 0 (max (+ 0 p4) 0)))
    (mk_synd_tup_Int_Int_Int_Int i700 i701 i702 i703)))))
(check-synth)
