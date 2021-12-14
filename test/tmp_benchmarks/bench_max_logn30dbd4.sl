(set-logic LIA)
(synth-fun s0 () Int)
(constraint (or (not true) (= 0 s0)))
(check-synth)
