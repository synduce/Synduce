(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$2 ((x107 synd_tup_Int_Int_Int_Int) (x108 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x107) (proj_synd_tup_Int_Int_Int_Int_1 x107)
    (proj_synd_tup_Int_Int_Int_Int_2 x107) (proj_synd_tup_Int_Int_Int_Int_3 x107)
    (proj_synd_tup_Int_Int_Int_Int_0 x108) (proj_synd_tup_Int_Int_Int_Int_1 x108)
    (proj_synd_tup_Int_Int_Int_Int_2 x108) (proj_synd_tup_Int_Int_Int_Int_3 x108) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p4 Int)
(declare-var i868 Int)
(declare-var i867 Int)
(declare-var i866 Int)
(declare-var i865 Int)
(constraint
 (or
  (not
   (and
    (and (and (and (and (>= i867 i865) (>= i868 i865)) (>= i868 i866)) (>= i868 i867)) (>= i866 0))
    (>= i866 (- i867))))
  (= i867
   (odot$2 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0) (mk_synd_tup_Int_Int_Int_Int i865 i866 i867 i868)))))
(constraint
 (or
  (not
   (and
    (and (and (and (and (>= i867 i865) (>= i868 i865)) (>= i868 i866)) (>= i868 i867)) (>= i866 0))
    (>= i866 (- i867))))
  (= (max (+ i867 p4) 0)
   (odot$2
    (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
     (max 0 (max (+ 0 p4) 0)))
    (mk_synd_tup_Int_Int_Int_Int i865 i866 i867 i868)))))
(check-synth)
