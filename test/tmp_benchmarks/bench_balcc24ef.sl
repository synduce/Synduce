(set-logic DTLIA)
(declare-datatype synd_tup_Int_Int_Bool
 ((mk_synd_tup_Int_Int_Bool (proj_synd_tup_Int_Int_Bool_0 Int) (proj_synd_tup_Int_Int_Bool_1 Int)
   (proj_synd_tup_Int_Int_Bool_2 Bool))))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))
(synth-fun odot$2 ((x21 synd_tup_Int_Int_Bool) (x22 synd_tup_Int_Int_Bool)) Bool
 ((IStart Bool) (Ipred Bool) (Ix Int) (Ic Int))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Int_Int_Bool_2 x21) (proj_synd_tup_Int_Int_Bool_2 x22) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred) (= Ix Ix) (> Ix Ix) 
    (>= Ix Ix)))
  (Ix Int
   (Ic (proj_synd_tup_Int_Int_Bool_0 x21) (proj_synd_tup_Int_Int_Bool_1 x21)
    (proj_synd_tup_Int_Int_Bool_0 x22) (proj_synd_tup_Int_Int_Bool_1 x22) 
    (- Ix) (+ Ix Ix) (min Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))))
(declare-var b7 Bool)
(declare-var b4 Bool)
(declare-var b0 Bool)
(declare-var b9 Bool)
(declare-var i10 Int)
(declare-var i9 Int)
(constraint
 (or (not (and (<= i10 i9) (and (<= i10 0) (= b9 (>= i10 0)))))
  (= b9 (odot$2 (mk_synd_tup_Int_Int_Bool 0 0 true) (mk_synd_tup_Int_Int_Bool i9 i10 b9)))))
(constraint
 (or (not (and (<= i10 i9) (and (<= i10 0) (= b9 (>= i10 0)))))
  (= (and b9 (>= (ite b0 (+ i9 1) (- i9 1)) 0))
   (odot$2
    (mk_synd_tup_Int_Int_Bool (ite b0 (+ 0 1) (- 0 1)) (min 0 (ite b0 (+ 0 1) (- 0 1)))
     (and true (>= (ite b0 (+ 0 1) (- 0 1)) 0)))
    (mk_synd_tup_Int_Int_Bool i9 i10 b9)))))
(constraint
 (or (not (and (<= i10 i9) (and (<= i10 0) (= b9 (>= i10 0)))))
  (= (and b9 (>= (ite b4 (+ i9 1) (- i9 1)) 0))
   (odot$2
    (mk_synd_tup_Int_Int_Bool (ite b4 (+ 0 1) (- 0 1)) (min 0 (ite b4 (+ 0 1) (- 0 1)))
     (and true (>= (ite b4 (+ 0 1) (- 0 1)) 0)))
    (mk_synd_tup_Int_Int_Bool i9 i10 b9)))))
(constraint
 (or (not (and (<= i10 i9) (and (<= i10 0) (= b9 (>= i10 0)))))
  (=
   (and (and b9 (>= (ite b7 (+ i9 1) (- i9 1)) 0))
    (>= (ite b4 (+ (ite b7 (+ i9 1) (- i9 1)) 1) (- (ite b7 (+ i9 1) (- i9 1)) 1)) 0))
   (odot$2
    (mk_synd_tup_Int_Int_Bool
     (ite b4 (+ (ite b7 (+ 0 1) (- 0 1)) 1) (- (ite b7 (+ 0 1) (- 0 1)) 1))
     (min (min 0 (ite b7 (+ 0 1) (- 0 1)))
      (ite b4 (+ (ite b7 (+ 0 1) (- 0 1)) 1) (- (ite b7 (+ 0 1) (- 0 1)) 1)))
     (and (and true (>= (ite b7 (+ 0 1) (- 0 1)) 0))
      (>= (ite b4 (+ (ite b7 (+ 0 1) (- 0 1)) 1) (- (ite b7 (+ 0 1) (- 0 1)) 1)) 0)))
    (mk_synd_tup_Int_Int_Bool i9 i10 b9)))))
(check-synth)
