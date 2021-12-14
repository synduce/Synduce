(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$3 ((x35 synd_tup_Int_Int_Int_Int) (x36 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x35) (proj_synd_tup_Int_Int_Int_Int_1 x35)
    (proj_synd_tup_Int_Int_Int_Int_2 x35) (proj_synd_tup_Int_Int_Int_Int_3 x35)
    (proj_synd_tup_Int_Int_Int_Int_0 x36) (proj_synd_tup_Int_Int_Int_Int_1 x36)
    (proj_synd_tup_Int_Int_Int_Int_2 x36) (proj_synd_tup_Int_Int_Int_Int_3 x36) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p4 Int)
(declare-var i329 Int)
(declare-var i328 Int)
(declare-var i327 Int)
(declare-var i326 Int)
(constraint
 (or (not (and (>= i328 i326) (>= i329 i326)))
  (= i329
   (odot$3 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0) (mk_synd_tup_Int_Int_Int_Int i326 i327 i328 i329)))))
(constraint
 (or (not (and (>= i328 i326) (>= i329 i326)))
  (= (max i329 (max (+ i328 p4) 0))
   (odot$3
    (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
     (max 0 (max (+ 0 p4) 0)))
    (mk_synd_tup_Int_Int_Int_Int i326 i327 i328 i329)))))
(check-synth)
