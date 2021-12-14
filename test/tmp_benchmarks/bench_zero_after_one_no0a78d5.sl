(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Bool_Bool
 ((mk_synd_tup_Bool_Bool_Bool (proj_synd_tup_Bool_Bool_Bool_0 Bool)
   (proj_synd_tup_Bool_Bool_Bool_1 Bool) (proj_synd_tup_Bool_Bool_Bool_2 Bool))))
(synth-fun odot$0 ((x29 synd_tup_Bool_Bool_Bool) (x30 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x29) (proj_synd_tup_Bool_Bool_Bool_1 x29)
    (proj_synd_tup_Bool_Bool_Bool_2 x29) (proj_synd_tup_Bool_Bool_Bool_0 x30)
    (proj_synd_tup_Bool_Bool_Bool_1 x30) (proj_synd_tup_Bool_Bool_Bool_2 x30) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun odot$1 ((x31 synd_tup_Bool_Bool_Bool) (x32 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x31) (proj_synd_tup_Bool_Bool_Bool_1 x31)
    (proj_synd_tup_Bool_Bool_Bool_2 x31) (proj_synd_tup_Bool_Bool_Bool_0 x32)
    (proj_synd_tup_Bool_Bool_Bool_1 x32) (proj_synd_tup_Bool_Bool_Bool_2 x32) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun s0$2 () Bool)
(synth-fun f0$2 ((x33 Bool)) Bool ((Ipred Bool))
 ((Ipred Bool (x33 (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b1 Bool)
(declare-var _elim_b1 Bool)
(declare-var _elim_b0 Bool)
(declare-var _elim_b Bool)
(constraint
 (= _elim_b
  (odot$0 (mk_synd_tup_Bool_Bool_Bool false false s0$2)
   (mk_synd_tup_Bool_Bool_Bool _elim_b _elim_b0 _elim_b1))))
(constraint
 (= _elim_b0
  (odot$1 (mk_synd_tup_Bool_Bool_Bool false false s0$2)
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
