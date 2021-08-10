(set-logic LIA)
(synth-fun c0 () Int
 ((Start Int (Ic (- Start) (+ Start Start) (ite Ipred Start Start))) (Ic Int ((Constant Int)))
  (Ipred Bool ((= Start Start) (> Start Start) (not Ipred) (and Ipred Ipred) (or Ipred Ipred)))))
(declare-var input Int)
(declare-var i Int)
(declare-var p Int)
(declare-var i3072 Int)
(declare-var i3073 Int)
(constraint
 (or
  (not
   (and
    (and (or (not (and (or (not (> input i)) (= i3072 0)) (> input i))) (> input p))
     (or (not (> input i)) (= i3072 0)))
    (> input i)))
  (= (+ (+ i3072 i3073) (ite (> p input) p 0)) c0)))
(check-synth)
