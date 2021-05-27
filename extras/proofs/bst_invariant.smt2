(set-logic ALL)
(set-option :quant-ind true)
(set-option :quant-cf true)
;;(set-option :full-saturate-quant true)

(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))

(declare-datatype Tree ((Leaf (value Int)) (Node (label Int) (left Tree) (right Tree))))

(declare-fun tmin (Tree) Int)
(assert (forall ((x Int)) (= (tmin (Leaf x)) x)))
(assert (forall ((a Int) (l Tree) (r Tree))
            (= (tmin (Node a l r)) (min (min a (tmin l)) (tmin r)))))

(declare-fun tmax (Tree) Int)
(assert (forall ((x Int)) (= (tmax (Leaf x)) x)))
(assert (forall ((a Int) (l Tree) (r Tree))
            (= (tmax (Node a l r)) (max (max a (tmax l)) (tmax r)))))


(declare-fun pred (Tree) Bool)
(declare-fun aux (Tree Int Int) Bool)
(assert (forall ((x Int)) (pred (Leaf x))))
(assert
    (forall ((a Int) (l Tree) (r Tree) (lo Int) (hi Int))
    (=
        (pred (Node a l r))
        (and (aux l (tmin l) a) (aux r a (tmax r)))
    )
))

(assert (forall ((a Int) (lo Int) (hi Int))
    (= (aux (Leaf a) lo hi) (and (<= lo a) (<= a hi)))))

(assert
    (forall ((a Int) (l Tree) (r Tree) (lo Int) (hi Int))
    (=
        (aux (Node a l r) lo hi)
        (and (<= lo a) (<= a hi) (aux l lo a) (aux r a hi))
    )
))

(declare-fun count-smaller (Int Tree) Int)
(assert (forall ((a Int) (x Int)) (= (count-smaller x (Leaf a)) (ite (<= a x) 1 0))))
(assert
    (forall ((a Int) (l Tree) (r Tree) (x Int))
        (=
            (count-smaller x (Node a l r))
            (+ (ite (<= a x) 1 0) (count-smaller x l) (count-smaller x r))
        )))

(declare-fun size (Tree) Int)
(assert (forall ((a Int)) (= (size (Leaf a)) 1)))
(assert (forall ((a Int) (l Tree) (r Tree)) (= (size (Node a l r)) (+ 1 (size l) (size r)))))

;; Proven
(assert
    ;(not
        (forall ((x Int) (l Tree))
            (=> (< x (tmin l))
                (= 0 (count-smaller x l))
            )
        )
    ;)
)

;; Proven
(assert
   ;; (not
        (forall ((x Int) (l Tree))
            (=> (>= x (tmax l))
               (= (size l) (count-smaller x l))
            )
        )
    ;)
)
;;
;;;; Not Proven ?
(assert
    (not
        (forall ((a Int) (l Tree) (r Tree))
            (=> (pred (Node a l r))
                (pred l)
            )
        )
    )
)


;; The lemma
;; (assert
;;     (not
;;         (forall
;;             ((x Int) (a Int) (l Tree) (r Tree))
;;             (=> (pred (Node a l r))
;;                 (=> (> a x)
;;                     (= 0 (count-smaller x r))
;;                 )
;;             )
;;         )
;;     )
;; )
(check-sat)