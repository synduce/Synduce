(set-logic DTLIA)
(declare-datatype synd_tup_Int_Bool
 ((mk_synd_tup_Int_Bool (proj_synd_tup_Int_Bool_0 Int) (proj_synd_tup_Int_Bool_1 Bool))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun f1$1 ((x110 synd_tup_Int_Bool)) Bool ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Bool_1 x110) (not Ipred) (and Ipred Ipred) (or Ipred Ipred) 
    (= Ix Ix) (> Ix Ix) (>= Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Bool_0 x110) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var i3078 Int)
(declare-var i1004 Int)
(declare-var i1005 Int)
(declare-var i3 Int)
(declare-var b14 Bool)
(declare-var i4805 Int)
(declare-var i4 Int)
(declare-var i Int)
(constraint
 (or
  (not
   (and
    (and (and (>= i4805 0) (= b14 (= i4805 0)))
     (and
      (and
       (and
        (and
         (and
          (and (or (not (<= i 0)) (> i (max i4 (+ i i))))
           (or (not (<= i 0)) (> i (max i4 (+ i i)))))
          (or (not (<= i 0)) (> i (max i4 (+ i i)))))
         (or (not (<= i 0)) (> i (max i4 (+ i i)))))
        (or (not (<= i 0)) (> i (max i4 (+ i i)))))
       (or (not (<= i 0)) (> i (max i4 (+ i i)))))
      (or (not (<= i 0)) (> i4805 i))))
    (<= i 0)))
  (= (and b14 (>= i4 0)) (f1$1 (mk_synd_tup_Int_Bool i4805 b14)))))
(constraint
 (or
  (not
   (and
    (and (and (>= i4805 0) (= b14 (= i4805 0)))
     (and
      (and
       (and
        (and (or (not (<= i 0)) (> i (+ i3 (ite (= i 0) i i1005))))
         (or (not (<= i 0)) (> i (+ i3 (ite (= i 0) i i1005)))))
        (or (not (<= i 0)) (> i (+ i3 (ite (= i 0) i i1005)))))
       (or (not (<= i 0)) (> i (min i3 (+ i (max i i1005))))))
      (or (not (<= i 0)) (> i4805 i))))
    (<= i 0)))
  (= (and b14 (>= (+ i3 i1005) 0)) (f1$1 (mk_synd_tup_Int_Bool i4805 b14)))))
(constraint
 (or
  (not
   (and
    (and (and (>= i4805 0) (= b14 (= i4805 0)))
     (and (or (not (<= i 0)) (> i4805 (ite (> i (+ i3 i1004)) i i3078)))
      (or (not (<= i 0)) (> i4805 i))))
    (<= i 0)))
  (= (and b14 (>= (+ i3 (+ i1004 i3078)) 0)) (f1$1 (mk_synd_tup_Int_Bool i4805 b14)))))
(check-synth)
