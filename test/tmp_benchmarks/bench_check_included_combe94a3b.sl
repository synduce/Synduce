(set-logic LIA)
(synth-fun xi_1 ((x57 Bool)) Bool ((IStart Bool) (Ipred Bool))
 ((IStart Bool ((or Ipred Ipred)))
  (Ipred Bool (x57 (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
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
    (and (or (not (and (> i0 hi) (< i lo))) (not b4)) (or (not (and (> i0 hi) (< i lo))) (> i0 i)))
    (and (> i0 hi) (< i lo))))
  (= (or (and (< lo i) (< i0 hi)) (or b4 b5)) (xi_1 b5))))
(check-synth)
