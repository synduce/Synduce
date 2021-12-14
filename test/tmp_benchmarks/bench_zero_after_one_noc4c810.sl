(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Bool_Bool
 ((mk_synd_tup_Bool_Bool_Bool (proj_synd_tup_Bool_Bool_Bool_0 Bool)
   (proj_synd_tup_Bool_Bool_Bool_1 Bool) (proj_synd_tup_Bool_Bool_Bool_2 Bool))))
(synth-fun odot$0 ((x44 synd_tup_Bool_Bool_Bool) (x45 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x44) (proj_synd_tup_Bool_Bool_Bool_1 x44)
    (proj_synd_tup_Bool_Bool_Bool_2 x44) (proj_synd_tup_Bool_Bool_Bool_0 x45)
    (proj_synd_tup_Bool_Bool_Bool_1 x45) (proj_synd_tup_Bool_Bool_Bool_2 x45) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun odot$1 ((x46 synd_tup_Bool_Bool_Bool) (x47 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x46) (proj_synd_tup_Bool_Bool_Bool_1 x46)
    (proj_synd_tup_Bool_Bool_Bool_2 x46) (proj_synd_tup_Bool_Bool_Bool_0 x47)
    (proj_synd_tup_Bool_Bool_Bool_1 x47) (proj_synd_tup_Bool_Bool_Bool_2 x47) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun s0$2 () Bool)
(synth-fun f0$2 ((x48 Bool)) Bool ((Ipred Bool))
 ((Ipred Bool (x48 (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
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
