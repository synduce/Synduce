(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Bool_Bool
 ((mk_synd_tup_Bool_Bool_Bool (proj_synd_tup_Bool_Bool_Bool_0 Bool)
   (proj_synd_tup_Bool_Bool_Bool_1 Bool) (proj_synd_tup_Bool_Bool_Bool_2 Bool))))
(synth-fun odot$0 ((x170 synd_tup_Bool_Bool_Bool) (x171 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x170) (proj_synd_tup_Bool_Bool_Bool_1 x170)
    (proj_synd_tup_Bool_Bool_Bool_2 x170) (proj_synd_tup_Bool_Bool_Bool_0 x171)
    (proj_synd_tup_Bool_Bool_Bool_1 x171) (proj_synd_tup_Bool_Bool_Bool_2 x171) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun odot$1 ((x172 synd_tup_Bool_Bool_Bool) (x173 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x172) (proj_synd_tup_Bool_Bool_Bool_1 x172)
    (proj_synd_tup_Bool_Bool_Bool_2 x172) (proj_synd_tup_Bool_Bool_Bool_0 x173)
    (proj_synd_tup_Bool_Bool_Bool_1 x173) (proj_synd_tup_Bool_Bool_Bool_2 x173) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun odot$2 ((x174 synd_tup_Bool_Bool_Bool) (x175 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x174) (proj_synd_tup_Bool_Bool_Bool_1 x174)
    (proj_synd_tup_Bool_Bool_Bool_2 x174) (proj_synd_tup_Bool_Bool_Bool_0 x175)
    (proj_synd_tup_Bool_Bool_Bool_1 x175) (proj_synd_tup_Bool_Bool_Bool_2 x175) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b8 Bool)
(declare-var b6 Bool)
(declare-var b7 Bool)
(declare-var b1 Bool)
(declare-var _elim_b31 Bool)
(declare-var _elim_b30 Bool)
(declare-var _elim_b29 Bool)
(constraint
 (= _elim_b29
  (odot$0 (mk_synd_tup_Bool_Bool_Bool false false false)
   (mk_synd_tup_Bool_Bool_Bool _elim_b29 _elim_b30 _elim_b31))))
(constraint
 (= _elim_b30
  (odot$1 (mk_synd_tup_Bool_Bool_Bool false false false)
   (mk_synd_tup_Bool_Bool_Bool _elim_b29 _elim_b30 _elim_b31))))
(constraint
 (= _elim_b29
  (odot$0
   (mk_synd_tup_Bool_Bool_Bool false false
    (odot$2 (mk_synd_tup_Bool_Bool_Bool false false false)
     (mk_synd_tup_Bool_Bool_Bool false false false)))
   (mk_synd_tup_Bool_Bool_Bool _elim_b29 _elim_b30 _elim_b31))))
(constraint
 (= _elim_b30
  (odot$1
   (mk_synd_tup_Bool_Bool_Bool false false
    (odot$2 (mk_synd_tup_Bool_Bool_Bool false false false)
     (mk_synd_tup_Bool_Bool_Bool false false false)))
   (mk_synd_tup_Bool_Bool_Bool _elim_b29 _elim_b30 _elim_b31))))
(constraint
 (= (or _elim_b29 b1)
  (odot$0 (mk_synd_tup_Bool_Bool_Bool (or false b1) (or (and false (not b1)) false) (not b1))
   (mk_synd_tup_Bool_Bool_Bool _elim_b29 _elim_b30 _elim_b31))))
(constraint
 (= (or (and _elim_b29 (not b1)) _elim_b30)
  (odot$1 (mk_synd_tup_Bool_Bool_Bool (or false b1) (or (and false (not b1)) false) (not b1))
   (mk_synd_tup_Bool_Bool_Bool _elim_b29 _elim_b30 _elim_b31))))
(constraint
 (= (or _elim_b29 b7)
  (odot$0
   (mk_synd_tup_Bool_Bool_Bool (or false b7) (or (and false (not b7)) false)
    (odot$2 (mk_synd_tup_Bool_Bool_Bool false false false)
     (mk_synd_tup_Bool_Bool_Bool b7 false (not b7))))
   (mk_synd_tup_Bool_Bool_Bool _elim_b29 _elim_b30 _elim_b31))))
(constraint
 (= (or (and _elim_b29 (not b7)) _elim_b30)
  (odot$1
   (mk_synd_tup_Bool_Bool_Bool (or false b7) (or (and false (not b7)) false)
    (odot$2 (mk_synd_tup_Bool_Bool_Bool false false false)
     (mk_synd_tup_Bool_Bool_Bool b7 false (not b7))))
   (mk_synd_tup_Bool_Bool_Bool _elim_b29 _elim_b30 _elim_b31))))
(constraint
 (= (or _elim_b29 b6)
  (odot$0
   (mk_synd_tup_Bool_Bool_Bool (or false b6) (or (and false (not b6)) false)
    (odot$2 (mk_synd_tup_Bool_Bool_Bool b6 false (not b6))
     (mk_synd_tup_Bool_Bool_Bool false false false)))
   (mk_synd_tup_Bool_Bool_Bool _elim_b29 _elim_b30 _elim_b31))))
(constraint
 (= (or (and _elim_b29 (not b6)) _elim_b30)
  (odot$1
   (mk_synd_tup_Bool_Bool_Bool (or false b6) (or (and false (not b6)) false)
    (odot$2 (mk_synd_tup_Bool_Bool_Bool b6 false (not b6))
     (mk_synd_tup_Bool_Bool_Bool false false false)))
   (mk_synd_tup_Bool_Bool_Bool _elim_b29 _elim_b30 _elim_b31))))
(constraint
 (= (or (or _elim_b29 b8) b6)
  (odot$0
   (mk_synd_tup_Bool_Bool_Bool (or (or false b8) b6)
    (or (and (or false b8) (not b6)) (or (and false (not b8)) false)) 
    (or (not b6) (not b8)))
   (mk_synd_tup_Bool_Bool_Bool _elim_b29 _elim_b30 _elim_b31))))
(constraint
 (= (or (and (or _elim_b29 b8) (not b6)) (or (and _elim_b29 (not b8)) _elim_b30))
  (odot$1
   (mk_synd_tup_Bool_Bool_Bool (or (or false b8) b6)
    (or (and (or false b8) (not b6)) (or (and false (not b8)) false)) 
    (or (not b6) (not b8)))
   (mk_synd_tup_Bool_Bool_Bool _elim_b29 _elim_b30 _elim_b31))))
(constraint
 (= (or (not b6) (not b8))
  (odot$2 (mk_synd_tup_Bool_Bool_Bool (or false b6) (or (and false (not b6)) false) (not b6))
   (mk_synd_tup_Bool_Bool_Bool (or false b8) (or (and false (not b8)) false) (not b8)))))
(check-synth)
