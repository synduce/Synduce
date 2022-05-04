(set-logic LIA) ; linear integer arithmetic

; existentially quantified variables:
; the function we want so synthesize
; ∃ f
(synth-fun f ((a Int) (b Int)) Int)

; universally quantified variables
(declare-var a Int) ; ∀ a ∈ ℤ
(declare-var b Int) ; ∀ b ∈ ℤ

; the constraints
(constraint (>= (f a b) a)); ; f(a,b) >= a
(constraint (>= (f a b) b)); ; f(a,b) >= b
(constraint (or
                (= (f a b) a)
                (= (f a b) b)))
; call the solver
(check-synth)
