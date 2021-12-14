(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Int_Int
 ((mk_synd_tup_Int_Int_Int_Int (proj_synd_tup_Int_Int_Int_Int_0 Int)
   (proj_synd_tup_Int_Int_Int_Int_1 Int) (proj_synd_tup_Int_Int_Int_Int_2 Int)
   (proj_synd_tup_Int_Int_Int_Int_3 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun odot$2 ((x153 synd_tup_Int_Int_Int_Int) (x154 synd_tup_Int_Int_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Int_Int_0 x153) (proj_synd_tup_Int_Int_Int_Int_1 x153)
    (proj_synd_tup_Int_Int_Int_Int_2 x153) (proj_synd_tup_Int_Int_Int_Int_3 x153)
    (proj_synd_tup_Int_Int_Int_Int_0 x154) (proj_synd_tup_Int_Int_Int_Int_1 x154)
    (proj_synd_tup_Int_Int_Int_Int_2 x154) (proj_synd_tup_Int_Int_Int_Int_3 x154) 
    (- Ix) (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var p4 Int)
(declare-var i963 Int)
(declare-var i962 Int)
(declare-var i961 Int)
(declare-var i960 Int)
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (and (and (>= i962 i960) (>= i963 i960)) (>= i963 i961)) (>= i963 i962))
       (>= i961 0))
      (>= i961 (- i962)))
     (>= i962 0))
    (>= i961 i960)))
  (= i962
   (odot$2 (mk_synd_tup_Int_Int_Int_Int 0 0 0 0) (mk_synd_tup_Int_Int_Int_Int i960 i961 i962 i963)))))
(constraint
 (or
  (not
   (and
    (and
     (and
      (and (and (and (and (>= i962 i960) (>= i963 i960)) (>= i963 i961)) (>= i963 i962))
       (>= i961 0))
      (>= i961 (- i962)))
     (>= i962 0))
    (>= i961 i960)))
  (= (max (+ i962 p4) 0)
   (odot$2
    (mk_synd_tup_Int_Int_Int_Int (+ 0 p4) (max 0 (+ 0 p4)) (max (+ 0 p4) 0)
     (max 0 (max (+ 0 p4) 0)))
    (mk_synd_tup_Int_Int_Int_Int i960 i961 i962 i963)))))
(check-synth)
