(set-logic DTLIA)
(declare-datatype Tuple_int_int
 ((mkT
   (proj0 Int)
   (proj1 Int))))

(synth-fun f0 ((x82 Int) (x83 Int) (x84 Tuple_int_int)) Int
 ((IStart Int) (Ix Int) (Ic Int) (Ipred Bool))
 ((IStart Int ((ite Ipred Ix Ix)))
  (Ix Int
   (Ic x82 x83 (proj0 x84) (proj1 x84) (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
  (Ic Int ((Constant Int)))
  (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))

(synth-fun f1 ((x85 Int) (x86 Int) (x87 Tuple_int_int)) Int
;   ((Ix Int) (Ic Int) (Ipred Bool))
;  ((Ix Int (Ic x85 x86 (proj0 x87) (proj1 x87) (- Ix) (+ Ix Ix) (ite Ipred Ix Ix)))
;   (Ic Int ((Constant Int)))
;   (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
)

(synth-fun s01 () Int
; ((Ix Int) (Ic Int) (Ipred Bool))
;  ((Ix Int (Ic (- Ix) (+ Ix Ix) (ite Ipred Ix Ix))) (Ic Int ((Constant Int)))
;   (Ipred Bool ((= Ix Ix) (> Ix Ix) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
)

(declare-var i3 Int)
(declare-var i1 Int)
(declare-var i0 Int)
(declare-var x Int)
(constraint (= (ite (= x i0) 0 0) (f0 x i0 (mkT 0 s01))))

(constraint
 (= (ite (= x i0) 1 (ite (= x i1) 0 0))
  (f0 x i0 (mkT (ite (= x i1) 0 0) (f1 x i1 (mkT 0 s01))))))

(constraint
 (= (ite (= x i0) 2 (ite (= x i1) 1 (ite (= x i3) 0 0)))
  (f0 x i0
   (mkT (ite (= x i1) (+ 1 0) (ite (= x i3) 0 0))
    (f1 x i1 (mkT (f0 x i3 (mkT 0 s01)) (f1 x i3 (mkT 0 s01))))))))

(check-synth)
