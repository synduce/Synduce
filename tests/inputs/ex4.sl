(set-logic LIA)

(declare-var x Int)

(define-fun max ((x Int) (y Int)) Int (ite (> x y) x y))

(synth-inv inv-f ((mts Int ) (sum Int )))

(define-fun pre-f ((mts Int ) (sum Int )) Bool (= mts 0))

(define-fun trans-f ((mts Int ) (sum Int ) (mtsp Int) (sump Int)) Bool
        (and (= mtsp (max 0 (+ mtsp x))) (= sump (+ sump x))))

(define-fun post-f ((mts Int) (sum Int )) Bool (< sum mts))

(inv-constraint inv-f pre-f trans-f post-f)

(check-synth)