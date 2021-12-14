(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Bool
 ((mk_synd_tup_Bool_Bool (proj_synd_tup_Bool_Bool_0 Bool) (proj_synd_tup_Bool_Bool_1 Bool))))
(synth-fun odot$1 ((x6 synd_tup_Bool_Bool) (x7 synd_tup_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_0 x6) (proj_synd_tup_Bool_Bool_1 x6) (proj_synd_tup_Bool_Bool_0 x7)
    (proj_synd_tup_Bool_Bool_1 x7) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b1 Bool)
(declare-var b3 Bool)
(declare-var b2 Bool)
(constraint (= b3 (odot$1 (mk_synd_tup_Bool_Bool false false) (mk_synd_tup_Bool_Bool b2 b3))))
(constraint
 (= (or (and b2 (not b1)) b3)
  (odot$1 (mk_synd_tup_Bool_Bool (or false b1) (or (and false (not b1)) false))
   (mk_synd_tup_Bool_Bool b2 b3))))
(check-synth)
