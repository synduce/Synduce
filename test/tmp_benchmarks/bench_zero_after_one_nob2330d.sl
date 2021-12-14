(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Bool_Bool
 ((mk_synd_tup_Bool_Bool_Bool (proj_synd_tup_Bool_Bool_Bool_0 Bool)
   (proj_synd_tup_Bool_Bool_Bool_1 Bool) (proj_synd_tup_Bool_Bool_Bool_2 Bool))))
(synth-fun odot$0 ((x34 synd_tup_Bool_Bool_Bool) (x35 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x34) (proj_synd_tup_Bool_Bool_Bool_1 x34)
    (proj_synd_tup_Bool_Bool_Bool_2 x34) (proj_synd_tup_Bool_Bool_Bool_0 x35)
    (proj_synd_tup_Bool_Bool_Bool_1 x35) (proj_synd_tup_Bool_Bool_Bool_2 x35) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun odot$1 ((x36 synd_tup_Bool_Bool_Bool) (x37 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x36) (proj_synd_tup_Bool_Bool_Bool_1 x36)
    (proj_synd_tup_Bool_Bool_Bool_2 x36) (proj_synd_tup_Bool_Bool_Bool_0 x37)
    (proj_synd_tup_Bool_Bool_Bool_1 x37) (proj_synd_tup_Bool_Bool_Bool_2 x37) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun f0$2 ((x38 Bool)) Bool ((Ipred Bool))
 ((Ipred Bool (x38 (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b1 Bool)
(declare-var _elim_b1 Bool)
(declare-var _elim_b0 Bool)
(declare-var _elim_b Bool)
(constraint
 (= _elim_b
  (odot$0 (mk_synd_tup_Bool_Bool_Bool false false false)
   (mk_synd_tup_Bool_Bool_Bool _elim_b _elim_b0 _elim_b1))))
(constraint
 (= _elim_b0
  (odot$1 (mk_synd_tup_Bool_Bool_Bool false false false)
   (mk_synd_tup_Bool_Bool_Bool _elim_b _elim_b0 _elim_b1))))
(constraint
 (= (or _elim_b b1)
  (odot$0 (mk_synd_tup_Bool_Bool_Bool (or false b1) (or (and false (not b1)) false) (f0$2 b1))
   (mk_synd_tup_Bool_Bool_Bool _elim_b _elim_b0 _elim_b1))))
(constraint
 (= (or (and _elim_b (not b1)) _elim_b0)
  (odot$1 (mk_synd_tup_Bool_Bool_Bool (or false b1) (or (and false (not b1)) false) (f0$2 b1))
   (mk_synd_tup_Bool_Bool_Bool _elim_b _elim_b0 _elim_b1))))
(check-synth)
