(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun f1$1 ((x91 synd_tup_Int_Bool)) Bool ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x91) (not Ipred) (and Ipred Ipred) (or Ipred Ipred) 
    (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Bool_0 x91) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i3078 Int)
(declare-var i1004 Int)
(declare-var i1005 Int)
(declare-var i3 Int)
(declare-var b12 Bool)
(declare-var i3082 Int)
(declare-var i4 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and
    (and (and (>= i3082 0) (= b12 (= i3082 0)))
     (and
      (and
       (and
        (and (or (not (<= i 0)) (> i (max i4 (+ i i)))) (or (not (<= i 0)) (> i (max i4 (+ i i)))))
        (or (not (<= i 0)) (> i (max i4 (+ i i)))))
       (or (not (<= i 0)) (> i (max i4 (+ i i)))))
      (or (not (<= i 0)) (> i3082 i))))
    (<= i 0)))
  (= (and b12 (>= i4 0)) (f1$1 (mk_synd_tup_Int_Bool i3082 b12)))))
(constraint
 (or
  (not
   (and
    (and (and (>= i3082 0) (= b12 (= i3082 0)))
     (and
      (and (or (not (<= i 0)) (> i (+ i3 (ite (= i 0) i i1005))))
       (or (not (<= i 0)) (> i (min i3 (+ i (max i i1005))))))
      (or (not (<= i 0)) (> i3082 i))))
    (<= i 0)))
  (= (and b12 (>= (+ i3 i1005) 0)) (f1$1 (mk_synd_tup_Int_Bool i3082 b12)))))
(constraint
 (or (not (and (and (>= i3082 0) (= b12 (= i3082 0))) (<= i 0)))
  (= (and b12 (>= (+ i3 (+ i1004 i3078)) 0)) (f1$1 (mk_synd_tup_Int_Bool i3082 b12)))))
(check-synth)
