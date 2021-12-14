(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int
 ((mk_synd_tup_Int_Int (proj_synd_tup_Int_Int_0 Int) (proj_synd_tup_Int_Int_1 Int))))
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(synth-fun join$0 ((x143 synd_tup_Int_Int) (x144 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((max Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x143) (proj_synd_tup_Int_Int_1 x143) 
    (proj_synd_tup_Int_Int_0 x144) (proj_synd_tup_Int_Int_1 x144) (- Ix) 
    (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun join$1 ((x145 synd_tup_Int_Int) (x146 synd_tup_Int_Int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((+ Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_0 x145) (proj_synd_tup_Int_Int_1 x145) 
    (proj_synd_tup_Int_Int_0 x146) (proj_synd_tup_Int_Int_1 x146) (- Ix) 
    (+ Ix Ix) (max Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var i7 Int)
(declare-var i5 Int)
(declare-var i6 Int)
(declare-var i Int)
(declare-var _elim_i20 Int)
(declare-var _elim_i19 Int)
(constraint
 (or (not (>= _elim_i19 0))
  (= _elim_i19 (join$0 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int _elim_i19 _elim_i20)))))
(constraint
 (or (not (>= _elim_i19 0))
  (= _elim_i19
   (join$0 (mk_synd_tup_Int_Int 0 (join$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int 0 0)))
    (mk_synd_tup_Int_Int _elim_i19 _elim_i20)))))
(constraint
 (or (not (>= _elim_i19 0))
  (= (max (+ _elim_i19 i) 0)
   (join$0 (mk_synd_tup_Int_Int (max (+ 0 i) 0) i) (mk_synd_tup_Int_Int _elim_i19 _elim_i20)))))
(constraint
 (or (not (>= _elim_i19 0))
  (= (max (+ _elim_i19 i6) 0)
   (join$0 (mk_synd_tup_Int_Int (max (+ 0 i6) 0) i6) (mk_synd_tup_Int_Int _elim_i19 _elim_i20)))))
(constraint
 (or (not (>= _elim_i19 0))
  (= (max (+ _elim_i19 i5) 0)
   (join$0 (mk_synd_tup_Int_Int (max (+ 0 i5) 0) i5) (mk_synd_tup_Int_Int _elim_i19 _elim_i20)))))
(constraint
 (or (not (>= _elim_i19 0))
  (= (max (+ (max (+ _elim_i19 i7) 0) i5) 0)
   (join$0 (mk_synd_tup_Int_Int (max (+ (max (+ 0 i7) 0) i5) 0) (+ i5 i7))
    (mk_synd_tup_Int_Int _elim_i19 _elim_i20)))))
(constraint
 (= (+ i5 i7)
  (join$1 (mk_synd_tup_Int_Int (max (+ 0 i5) 0) i5) (mk_synd_tup_Int_Int (max (+ 0 i7) 0) i7))))
(constraint (= i5 (join$1 (mk_synd_tup_Int_Int (max (+ 0 i5) 0) i5) (mk_synd_tup_Int_Int 0 0))))
(constraint (= i6 (join$1 (mk_synd_tup_Int_Int 0 0) (mk_synd_tup_Int_Int (max (+ 0 i6) 0) i6))))
(check-synth)
