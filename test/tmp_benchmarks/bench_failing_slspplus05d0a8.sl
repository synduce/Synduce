(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun f1$0 ((x76 synd_tup_Int_Bool)) Int ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Bool_0 x76) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x76) (= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred)
    (or Ipred Ipred)))))
(declare-var i1005 Int)
(declare-var i3 Int)
(declare-var b9 Bool)
(declare-var i3068 Int)
(declare-var i4 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and
    (and (and (>= i3068 0) (= b9 (= i3068 0)))
     (and
      (and
       (and
        (and (or (not (<= i 0)) (> i (max i4 (+ i i)))) (or (not (<= i 0)) (> i (max i4 (+ i i)))))
        (or (not (<= i 0)) (> i (max i4 (+ i i)))))
       (or (not (<= i 0)) (> i (max i4 (+ i i)))))
      (or (not (<= i 0)) (> i3068 i))))
    (<= i 0)))
  (= (ite (and (>= i4 0) b9) (+ i3068 i4) 0) (f1$0 (mk_synd_tup_Int_Bool i3068 b9)))))
(constraint
 (or
  (not
   (and
    (and (and (>= i3068 0) (= b9 (= i3068 0)))
     (and
      (and (or (not (<= i 0)) (> i (+ i3 (ite (= i 0) i i1005))))
       (or (not (<= i 0)) (> i (min i3 (+ i (max i i1005))))))
      (or (not (<= i 0)) (> i3068 i))))
    (<= i 0)))
  (= (ite (and (>= (+ i3 i1005) 0) b9) (+ i3068 (+ i3 i1005)) 0)
   (f1$0 (mk_synd_tup_Int_Bool i3068 b9)))))
(check-synth)
