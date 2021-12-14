(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Bool_Bool
 ((mk_synd_tup_Bool_Bool_Bool (proj_synd_tup_Bool_Bool_Bool_0 Bool)
   (proj_synd_tup_Bool_Bool_Bool_1 Bool) (proj_synd_tup_Bool_Bool_Bool_2 Bool))))
(synth-fun odot$0 ((x29 synd_tup_Bool_Bool_Bool) (x30 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((and Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x29) (proj_synd_tup_Bool_Bool_Bool_1 x29)
    (proj_synd_tup_Bool_Bool_Bool_2 x29) (proj_synd_tup_Bool_Bool_Bool_0 x30)
    (proj_synd_tup_Bool_Bool_Bool_1 x30) (proj_synd_tup_Bool_Bool_Bool_2 x30) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b8 Bool)
(declare-var b7 Bool)
(declare-var b14 Bool)
(declare-var b13 Bool)
(declare-var b12 Bool)
(declare-var b0 Bool)
(constraint
 (= (and b0 b12)
  (odot$0 (mk_synd_tup_Bool_Bool_Bool b0 true b0) (mk_synd_tup_Bool_Bool_Bool b12 b13 b14))))
(constraint
 (= (and b7 (and b8 b12))
  (odot$0 (mk_synd_tup_Bool_Bool_Bool (and b7 b8) (and (or (not b7) b8) true) b8)
   (mk_synd_tup_Bool_Bool_Bool b12 b13 b14))))
(check-synth)
