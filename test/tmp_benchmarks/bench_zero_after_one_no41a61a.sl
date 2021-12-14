(set-logic DTLIA)
(declare-datatype synd_tup_Bool_Bool_Bool
 ((mk_synd_tup_Bool_Bool_Bool (proj_synd_tup_Bool_Bool_Bool_0 Bool)
   (proj_synd_tup_Bool_Bool_Bool_1 Bool) (proj_synd_tup_Bool_Bool_Bool_2 Bool))))
(synth-fun odot$0 ((x176 synd_tup_Bool_Bool_Bool) (x177 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x176) (proj_synd_tup_Bool_Bool_Bool_1 x176)
    (proj_synd_tup_Bool_Bool_Bool_2 x176) (proj_synd_tup_Bool_Bool_Bool_0 x177)
    (proj_synd_tup_Bool_Bool_Bool_1 x177) (proj_synd_tup_Bool_Bool_Bool_2 x177) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun odot$1 ((x178 synd_tup_Bool_Bool_Bool) (x179 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x178) (proj_synd_tup_Bool_Bool_Bool_1 x178)
    (proj_synd_tup_Bool_Bool_Bool_2 x178) (proj_synd_tup_Bool_Bool_Bool_0 x179)
    (proj_synd_tup_Bool_Bool_Bool_1 x179) (proj_synd_tup_Bool_Bool_Bool_2 x179) 
    (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(synth-fun odot$2 ((x180 synd_tup_Bool_Bool_Bool) (x181 synd_tup_Bool_Bool_Bool)) Bool
 ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool
   ((proj_synd_tup_Bool_Bool_Bool_0 x180) (proj_synd_tup_Bool_Bool_Bool_1 x180)
    (proj_synd_tup_Bool_Bool_Bool_2 x180) (proj_synd_tup_Bool_Bool_Bool_0 x181)
    (proj_synd_tup_Bool_Bool_Bool_1 x181) (proj_synd_tup_Bool_Bool_Bool_2 x181) 
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
