(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s0$1 () Int)
(synth-fun oplus$0 ((x30 synd_tup_Int_Int) (x31 Int)) Int ((IStart Int) (Ix Int) (Ic Int))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x30) (proj_synd_tup_Int_Int_1 x30) x31 
    (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int ((Constant Int)))))
(synth-fun oplus$1 ((x32 synd_tup_Int_Int) (x33 Int)) Int)
(declare-var p1 Int)
(declare-var p Int)
(constraint (= (max p 0) (oplus$0 (mk_synd_tup_Int_Int 0 s0$1) p)))
(constraint
 (= (max (+ (max p1 0) p) 0)
  (oplus$0 (mk_synd_tup_Int_Int (max p 0) (oplus$1 (mk_synd_tup_Int_Int 0 s0$1) p)) p1)))
(check-synth)
