(set-logic LIA)
(declare-var i16 Int)
(declare-var i14 Int)
(declare-var i5 Int)
(declare-var i1 Int)
(constraint
 (or (not (and (and (< i5 i16) (< i1 i14)) (= i1 i5)))
  (= (or (or (= i5 i1) (= i16 i1)) (or (= i14 i5) (= i16 i14))) false)))
(check-synth)
