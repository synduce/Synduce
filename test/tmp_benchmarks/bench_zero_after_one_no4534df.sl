(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Bool_Bool
 ((mk_synd_tup_Bool_Bool_Bool (proj_synd_tup_Bool_Bool_Bool_0 Bool)
   (proj_synd_tup_Bool_Bool_Bool_1 Bool) (proj_synd_tup_Bool_Bool_Bool_2 Bool))))
(synth-fun odot$0 ((x96 synd_tup_Bool_Bool_Bool) (x97 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x96) (proj_synd_tup_Bool_Bool_Bool_1 x96)
    (proj_synd_tup_Bool_Bool_Bool_2 x96) (proj_synd_tup_Bool_Bool_Bool_0 x97)
    (proj_synd_tup_Bool_Bool_Bool_1 x97) (proj_synd_tup_Bool_Bool_Bool_2 x97) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun odot$1 ((x98 synd_tup_Bool_Bool_Bool) (x99 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x98) (proj_synd_tup_Bool_Bool_Bool_1 x98)
    (proj_synd_tup_Bool_Bool_Bool_2 x98) (proj_synd_tup_Bool_Bool_Bool_0 x99)
    (proj_synd_tup_Bool_Bool_Bool_1 x99) (proj_synd_tup_Bool_Bool_Bool_2 x99) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun odot$2 ((x100 synd_tup_Bool_Bool_Bool) (x101 synd_tup_Bool_Bool_Bool)) Bool
 ((Ipred Bool))
 ((Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x100) (proj_synd_tup_Bool_Bool_Bool_1 x100)
    (proj_synd_tup_Bool_Bool_Bool_2 x100) (proj_synd_tup_Bool_Bool_Bool_0 x101)
    (proj_synd_tup_Bool_Bool_Bool_1 x101) (proj_synd_tup_Bool_Bool_Bool_2 x101) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b7 Bool)
(declare-var b1 Bool)
(declare-var _elim_b13 Bool)
(declare-var _elim_b12 Bool)
(declare-var _elim_b11 Bool)
(constraint
 (= _elim_b11
  (odot$0 (mk_synd_tup_Bool_Bool_Bool false false false)
   (mk_synd_tup_Bool_Bool_Bool _elim_b11 _elim_b12 _elim_b13))))
(constraint
 (= _elim_b12
  (odot$1 (mk_synd_tup_Bool_Bool_Bool false false false)
   (mk_synd_tup_Bool_Bool_Bool _elim_b11 _elim_b12 _elim_b13))))
(constraint
 (= _elim_b11
  (odot$0
   (mk_synd_tup_Bool_Bool_Bool false false
    (odot$2 (mk_synd_tup_Bool_Bool_Bool false false false)
     (mk_synd_tup_Bool_Bool_Bool false false false)))
   (mk_synd_tup_Bool_Bool_Bool _elim_b11 _elim_b12 _elim_b13))))
(constraint
 (= _elim_b12
  (odot$1
   (mk_synd_tup_Bool_Bool_Bool false false
    (odot$2 (mk_synd_tup_Bool_Bool_Bool false false false)
     (mk_synd_tup_Bool_Bool_Bool false false false)))
   (mk_synd_tup_Bool_Bool_Bool _elim_b11 _elim_b12 _elim_b13))))
(constraint
 (= (or _elim_b11 b1)
  (odot$0 (mk_synd_tup_Bool_Bool_Bool (or false b1) (or (and false (not b1)) false) (not b1))
   (mk_synd_tup_Bool_Bool_Bool _elim_b11 _elim_b12 _elim_b13))))
(constraint
 (= (or (and _elim_b11 (not b1)) _elim_b12)
  (odot$1 (mk_synd_tup_Bool_Bool_Bool (or false b1) (or (and false (not b1)) false) (not b1))
   (mk_synd_tup_Bool_Bool_Bool _elim_b11 _elim_b12 _elim_b13))))
(constraint
 (= (or _elim_b11 b7)
  (odot$0
   (mk_synd_tup_Bool_Bool_Bool (or false b7) (or (and false (not b7)) false)
    (odot$2 (mk_synd_tup_Bool_Bool_Bool false false false)
     (mk_synd_tup_Bool_Bool_Bool b7 false (not b7))))
   (mk_synd_tup_Bool_Bool_Bool _elim_b11 _elim_b12 _elim_b13))))
(constraint
 (= (or (and _elim_b11 (not b7)) _elim_b12)
  (odot$1
   (mk_synd_tup_Bool_Bool_Bool (or false b7) (or (and false (not b7)) false)
    (odot$2 (mk_synd_tup_Bool_Bool_Bool false false false)
     (mk_synd_tup_Bool_Bool_Bool b7 false (not b7))))
   (mk_synd_tup_Bool_Bool_Bool _elim_b11 _elim_b12 _elim_b13))))
(check-synth)
