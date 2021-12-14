(set-logic LIA)
(synth-fun xi_11 ((x62 Bool) (x63 Bool)) Bool ((Ipred Bool))
 ((Ipred Bool (x62 x63 (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var b5 Bool)
(declare-var b4 Bool)
(declare-var i0 Int)
(declare-var i Int)
(declare-var lo Int)
(declare-var hi Int)
(constraint
 (or
  (not
   (and
    (and
     (and (or (not (and (> i0 hi) (not (> i lo)))) (not b4))
      (or (not (and (> i0 hi) (not (> i lo)))) (> i0 i)))
     (> i0 hi))
    (> i lo)))
  (= (or (and (< lo i) (< i0 hi)) (or b4 b5)) (xi_11 b5 b4))))
(check-synth)
