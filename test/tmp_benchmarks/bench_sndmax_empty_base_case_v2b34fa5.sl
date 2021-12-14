(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$0 ((x78 synd_tup_Int_Int) (x79 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x78) (proj_synd_tup_Int_Int_1 x78) (proj_synd_tup_Int_Int_0 x79)
    (proj_synd_tup_Int_Int_1 x79) (- Ix) (+ Ix Ix) (min Ix Ix) (max Ix Ix) 
    (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i707 Int)
(declare-var i697 Int)
(declare-var i0 Int)
(declare-var i1823 Int)
(declare-var i1822 Int)
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (>= (+ i1822 i1823) (max i1822 i1823)) (>= (+ i1822 i1823) (max i1822 i1823)))
       (>= (+ i1822 i1823) (max i1822 i1823)))
      (>= (+ i1822 i1823) (max i1822 i1823)))
     (>= i1823 0))
    (>= i1822 (- i1823))))
  (= i1822 (odot$0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int i1822 i1823)))))
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (>= (+ i1822 i1823) (max i1822 i1823)) (>= (+ i1822 i1823) (max i1822 i1823)))
       (>= (+ i1822 i1823) (max i1822 i1823)))
      (>= (+ i1822 i1823) (max i1822 i1823)))
     (>= i1823 0))
    (>= i1822 (- i1823))))
  (= (max i0 i1822)
   (odot$0 (mk_synd_tup_Int_Int (max i0 0) (max 0 (min i0 0))) (mk_synd_tup_Int_Int i1822 i1823)))))
(constraint
 (or (not (and (and (>= i1823 0) (>= i1822 (- i1823))) (>= i1822 0)))
  (= (max i697 i1822)
   (odot$0 (mk_synd_tup_Int_Int (max i697 0) (max 0 (min i697 0)))
    (mk_synd_tup_Int_Int i1822 i1823)))))
(constraint
 (or (not (and (and (>= i1823 0) (>= i1822 (- i1823))) (>= i1822 0)))
  (= (max i697 (max i707 i1822))
   (odot$0
    (mk_synd_tup_Int_Int (max i697 (max i707 0))
     (max (max 0 (min i707 0)) (min i697 (max i707 0))))
    (mk_synd_tup_Int_Int i1822 i1823)))))
(check-synth)
