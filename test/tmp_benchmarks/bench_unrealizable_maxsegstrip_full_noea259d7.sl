(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun s1$1 ((x141 synd_tup_Int_Int_Int_Int) (x142 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x141) (proj_synd_tup_Int_Int_Int_Int_1 x141)
    (proj_synd_tup_Int_Int_Int_Int_2 x141) (proj_synd_tup_Int_Int_Int_Int_3 x141)
    (proj_synd_tup_Int_Int_Int_Int_0 x142) (proj_synd_tup_Int_Int_Int_Int_1 x142)
    (proj_synd_tup_Int_Int_Int_Int_2 x142) (proj_synd_tup_Int_Int_Int_Int_3 x142) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i34 Int)
(declare-var i22 Int)
(declare-var i2910 Int)
(declare-var i2909 Int)
(declare-var i2908 Int)
(declare-var i2907 Int)
(declare-var i2 Int)
(constraint
 (or (not (and (and (>= i2909 i2910) (>= i2908 i2909)) (>= i2908 i2907)))
  (= (max i2908 (max (+ i2907 i2) 0))
   (s1$1 (mk_synd_tup_Int_Int_Int_Int i2907 i2908 i2909 i2910)
    (mk_synd_tup_Int_Int_Int_Int (max i2 0) (max i2 0) (max i2 0) i2)))))
(constraint
 (or (not (and (and (>= i2909 i2910) (>= i2908 i2909)) (>= i2908 i2907)))
  (= (max (max i2908 (max (+ i2907 i34) 0)) (max (+ (max (+ i2907 i34) 0) i22) 0))
   (s1$1 (mk_synd_tup_Int_Int_Int_Int i2907 i2908 i2909 i2910)
    (mk_synd_tup_Int_Int_Int_Int (max (+ (max i34 0) i22) 0)
     (max (max i34 0) (max (+ (max i34 0) i22) 0)) (max (+ i34 i22) (max i34 0)) 
     (+ i34 i22))))))
(check-synth)
