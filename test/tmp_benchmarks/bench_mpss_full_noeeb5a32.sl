(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$0 ((x61 synd_tup_Int_Int) (x62 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x61) (proj_synd_tup_Int_Int_1 x61) (proj_synd_tup_Int_Int_0 x62)
    (proj_synd_tup_Int_Int_1 x62) (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i24 Int)
(declare-var i14 Int)
(declare-var i524 Int)
(declare-var i523 Int)
(declare-var i2 Int)
(constraint
 (or (not (>= i523 i524))
  (= (max (+ i524 i2) i523)
   (s1$0 (mk_synd_tup_Int_Int i523 i524) (mk_synd_tup_Int_Int (max 0 i2) i2)))))
(constraint
 (or (not (>= i523 i524))
  (= (max (+ (+ i524 i24) i14) (max (+ i524 i24) i523))
   (s1$0 (mk_synd_tup_Int_Int i523 i524)
    (mk_synd_tup_Int_Int (max (+ i24 i14) (max 0 i24)) (+ i24 i14))))))
(check-synth)
