(set-logic LIA)
(synth-fun c0 () Int)
(declare-var i4481 Int)
(declare-var i4480 Int)
(declare-var p Int)
(declare-var i Int)
(declare-var input Int)
(constraint
 (or
  (not
   (and (and (or (not (> input i)) (> input p)) (or (not (> input i)) (= i4480 0))) (> input i)))
  (= (+ (+ i4480 i4481) (ite (> p input) p 0)) c0)))
(check-synth)
