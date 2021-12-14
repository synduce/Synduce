(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun oplus$0 ((x66 Int) (x67 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic x66 (proj_synd_tup_Int_Int_0 x67) (proj_synd_tup_Int_Int_1 x67) 
    (- Ix) (+ Ix Ix) (min Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i1 Int)
(declare-var i0 Int)
(declare-var i Int)
(constraint (or (not (and (< i 0) (and true true))) (= 0 (oplus$0 i (mk_synd_tup_Int_Int 0 0)))))
(constraint
 (or (not (and (< i 0) (and (and (< i0 0) (and true true)) (and (not (= i i0)) true))))
  (= (ite (< i i0) i0 i) (oplus$0 i (mk_synd_tup_Int_Int 0 i0)))))
(constraint
 (or
  (not
   (and (< i 0)
    (and (and (< i0 0) (and (and (< i1 0) (and true true)) (and (not (= i0 i1)) true)))
     (and (not (= i i0)) (and (not (= i i1)) true)))))
  (= (ite (< i0 i1) (ite (< i i0) i0 (ite (< i i1) i i1)) (ite (< i i1) i1 (ite (< i i0) i i0)))
   (oplus$0 i (mk_synd_tup_Int_Int (ite (< i0 i1) i1 i0) (min i0 i1))))))
(check-synth)
