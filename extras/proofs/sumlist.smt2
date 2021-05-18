(set-logic ALL)
(set-option :incremental true)
(set-option :quant-ind true)

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

(declare-fun repr (IntCList) IntList)
(declare-fun dec (IntCList IntCList) IntList)
(assert (= (repr CEmpty) Empty))
(assert (forall ((x Int)) (= (repr (CElt x)) (Cons x Empty))))

(assert (forall ((x IntCList) (y IntCList)) (= (repr (CConcat x y)) (dec y x))))
(assert (forall ((x IntCList) (y IntCList) (z IntCList))
    (=
        (dec x (CConcat y z))
        (dec (CConcat y x) z)
    )
))
(assert (forall ((x IntCList)) (= (dec x CEmpty) (repr x))))
(assert (forall ((x IntCList) (a Int)) (= (dec x (CElt a)) (Cons a (repr x)))))

;; Base case proven
;; (assert (not (= (f (repr CEmpty)) (g CEmpty))))
(assert (= (f (repr CEmpty)) (g CEmpty)))
;; Case cases #2 proven
;; (assert (not (forall ((x Int)) (= (f (repr (CElt x))) (g (CElt x))))))
(assert (forall ((x Int)) (= (f (repr (CElt x))) (g (CElt x)))))

;; Proven
(assert
     ;;(not
         (forall ((x IntCList) (a Int))
             (= (g (CConcat x (CElt a))) (+ (g x) a)))
    ;;)
)

(assert
     (not
         (forall ((x IntCList) (a Int))
             (= (g (CConcat x (CElt a))) (+ (f (repr x)) a)))
    )
)


(check-sat)