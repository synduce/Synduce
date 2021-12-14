(set-logic LIA)
(declare-var i8854 Int)
(declare-var i8853 Int)
(declare-var p Int)
(declare-var i Int)
(declare-var input Int)
(constraint
 (or
  (not
   (and
    (and (and (or (not (> input i)) (= i8853 i8854)) (or (not (> input i)) (> input p)))
     (or (not (> input i)) (= i8853 0)))
    (> input i)))
  (= (+ (+ i8853 i8854) (ite (> p input) p 0)) 1)))
(check-synth)
