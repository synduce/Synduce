(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Bool_Bool
 ((mk_synd_tup_Bool_Bool_Bool (proj_synd_tup_Bool_Bool_Bool_0 Bool)
   (proj_synd_tup_Bool_Bool_Bool_1 Bool) (proj_synd_tup_Bool_Bool_Bool_2 Bool))))
(synth-fun odot$2 ((x25 synd_tup_Bool_Bool_Bool) (x26 synd_tup_Bool_Bool_Bool)) Bool 
 ((Ipred Bool))
 ((Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x25) (proj_synd_tup_Bool_Bool_Bool_1 x25)
    (proj_synd_tup_Bool_Bool_Bool_2 x25) (proj_synd_tup_Bool_Bool_Bool_0 x26)
    (proj_synd_tup_Bool_Bool_Bool_1 x26) (proj_synd_tup_Bool_Bool_Bool_2 x26) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(check-synth)
