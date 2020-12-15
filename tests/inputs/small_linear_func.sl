(set-logic LIA)

(define-fun max ((x Int) (y Int)) Int (ite (> x y) x y))

(synth-fun f ((x Int) (y Int)) Int
	((I Int) (C Int))
	((I Int (x y (+ I I) (max I C) (* I C)))
	 (C Int ((Constant Int) 100 1000 10000)))
)

(define-fun func ((x Int)) Int (+ (* x 100) 1000))

(declare-var x Int)
(declare-var y Int)


(constraint (= (f x y) (f y x)))
(constraint (and (<= (func x) (f x y))
				(<= (func y) (f x y))))


(check-synth)

