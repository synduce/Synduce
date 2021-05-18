(set-logic ALL)

(declare-datatypes ((IntList 0))
    ((
        (Empty)
        (Cons (head Int) (tail IntList))
    ))
)

(declare-datatype IntCList
    (
        (CEmpty)
        (CElt (elt Int))
        (CConcat (left IntCList) (right IntCList))
    )
)

(declare-fun f (IntList) Int)
(assert (= (f Empty) 0))
(assert (forall ((hd Int) (tl IntList))
            (= (f (Cons hd tl)) (+ hd (f tl)))))


(declare-fun g (IntCList) Int)
(assert (= (g CEmpty) 0))
(assert (forall ((x Int)) (= (g (CElt x)) x)))
(assert (forall ((x IntCList) (y IntCList))
    (= (g (CConcat x y)) (+ (g x) (g y)))
))

(assert (forall ((x Int)) (= x (+ x 0))))
(assert (forall ((x Int)) (= (f (Cons x Empty)) (g (CElt x)))))

(check-sat)

;;(get-model)