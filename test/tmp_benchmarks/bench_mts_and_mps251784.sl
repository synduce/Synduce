(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int (proj_synd_tup_Int_Int_Int_0 Int) (proj_synd_tup_Int_Int_Int_1 Int)
   (proj_synd_tup_Int_Int_Int_2 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun oplus$2 ((x12 Int) (x13 synd_tup_Int_Int_Int)) Int ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic x12 (proj_synd_tup_Int_Int_Int_0 x13) (proj_synd_tup_Int_Int_Int_1 x13)
    (proj_synd_tup_Int_Int_Int_2 x13) (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var p1 Int)
(declare-var p Int)
(constraint (= (max p 0) (oplus$2 p (mk_synd_tup_Int_Int_Int 0 0 0))))
(constraint
 (= (max (+ (max p1 0) p) 0)
  (oplus$2 p (mk_synd_tup_Int_Int_Int (+ p1 0) (max (+ 0 p1) 0) (max (+ 0 p1) 0)))))
(check-synth)
