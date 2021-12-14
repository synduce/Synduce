(set-logic LIA)
(synth-fun xi_12 ((x65 Bool)) Bool ((Ipred Bool))
 ((Ipred Bool (x65 (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
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
    (not (> i lo))))
  (= (or (and (< lo i) (< i0 hi)) (or b4 b5)) (xi_12 b5))))
(check-synth)
