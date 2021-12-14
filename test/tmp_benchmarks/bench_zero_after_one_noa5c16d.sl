(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Bool_Bool
 ((mk_synd_tup_Bool_Bool_Bool (proj_synd_tup_Bool_Bool_Bool_0 Bool)
   (proj_synd_tup_Bool_Bool_Bool_1 Bool) (proj_synd_tup_Bool_Bool_Bool_2 Bool))))
(synth-fun odot$0 ((x39 synd_tup_Bool_Bool_Bool) (x40 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x39) (proj_synd_tup_Bool_Bool_Bool_1 x39)
    (proj_synd_tup_Bool_Bool_Bool_2 x39) (proj_synd_tup_Bool_Bool_Bool_0 x40)
    (proj_synd_tup_Bool_Bool_Bool_1 x40) (proj_synd_tup_Bool_Bool_Bool_2 x40) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun odot$1 ((x41 synd_tup_Bool_Bool_Bool) (x42 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x41) (proj_synd_tup_Bool_Bool_Bool_1 x41)
    (proj_synd_tup_Bool_Bool_Bool_2 x41) (proj_synd_tup_Bool_Bool_Bool_0 x42)
    (proj_synd_tup_Bool_Bool_Bool_1 x42) (proj_synd_tup_Bool_Bool_Bool_2 x42) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun f0$2 ((x43 Bool)) Bool ((Ipred Bool))
 ((Ipred Bool (x43 (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b1 Bool)
(declare-var _elim_b1 Bool)
(declare-var _elim_b0 Bool)
(declare-var _elim_b Bool)
(constraint
 (= _elim_b
  (odot$0 (mk_synd_tup_Bool_Bool_Bool false false true)
   (mk_synd_tup_Bool_Bool_Bool _elim_b _elim_b0 _elim_b1))))
(constraint
 (= _elim_b0
  (odot$1 (mk_synd_tup_Bool_Bool_Bool false false true)
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
