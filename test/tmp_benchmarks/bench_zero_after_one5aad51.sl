(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Bool_Bool
 ((mk_synd_tup_Bool_Bool_Bool (proj_synd_tup_Bool_Bool_Bool_0 Bool)
   (proj_synd_tup_Bool_Bool_Bool_1 Bool) (proj_synd_tup_Bool_Bool_Bool_2 Bool))))
(synth-fun odot$1 ((x25 synd_tup_Bool_Bool_Bool) (x26 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x25) (proj_synd_tup_Bool_Bool_Bool_1 x25)
    (proj_synd_tup_Bool_Bool_Bool_2 x25) (proj_synd_tup_Bool_Bool_Bool_0 x26)
    (proj_synd_tup_Bool_Bool_Bool_1 x26) (proj_synd_tup_Bool_Bool_Bool_2 x26) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b16 Bool)
(declare-var b11 Bool)
(declare-var b1 Bool)
(declare-var b22 Bool)
(declare-var b21 Bool)
(declare-var b20 Bool)
(constraint
 (= b21
  (odot$1 (mk_synd_tup_Bool_Bool_Bool false false false) (mk_synd_tup_Bool_Bool_Bool b20 b21 b22))))
(constraint
 (= (or (and b20 (not b1)) b21)
  (odot$1
   (mk_synd_tup_Bool_Bool_Bool (or false b1) (or (and false (not b1)) false) (or false (not b1)))
   (mk_synd_tup_Bool_Bool_Bool b20 b21 b22))))
(constraint
 (= (or (and b20 (not b11)) b21)
  (odot$1
   (mk_synd_tup_Bool_Bool_Bool (or false b11) (or (and false (not b11)) false)
    (or false (not b11)))
   (mk_synd_tup_Bool_Bool_Bool b20 b21 b22))))
(constraint
 (= (or (and (or b20 b16) (not b11)) (or (and b20 (not b16)) b21))
  (odot$1
   (mk_synd_tup_Bool_Bool_Bool (or (or false b16) b11)
    (or (and (or false b16) (not b11)) (or (and false (not b16)) false))
    (or (or false (not b16)) (not b11)))
   (mk_synd_tup_Bool_Bool_Bool b20 b21 b22))))
(check-synth)
