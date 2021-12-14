(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join$0 ((x21 synd_tup_Int_Int) (x22 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x21) (proj_synd_tup_Int_Int_1 x21) (proj_synd_tup_Int_Int_0 x22)
    (proj_synd_tup_Int_Int_1 x22) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i Int)
(declare-var i160 Int)
(declare-var i159 Int)
(constraint
 (or (not (>= i159 i160))
  (= i159 (join$0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i159 i160)))))
(constraint
 (or (not (>= i159 i160))
  (= (max (+ i159 i) 0)
   (join$0 (mk_synd_tup_Int_Int (max (+ 0 i) 0) (+ 0 i)) (mk_synd_tup_Int_Int i159 i160)))))
(check-synth)
