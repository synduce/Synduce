(set-logic UFDTLIA)


(declare-datatype List ((Cons (hd Int) (tl List)) (Nil)))

(define-fun-rec contains ((l List) (x Int)) Bool
    (match l
        ((Nil false)
        ((Cons y z) (or (= x y) (contains z x))))))

(define-fun-rec set-includes ((l1 List) (l2 List)) Bool
    (match l1
        ((Nil true)
        ((Cons x y) (and (contains l2 x) (set-includes y l2)))))
)

(define-fun set-equals ((l1 List) (l2 List)) Bool
    (and (set-includes l1 l2) (set-includes l2 l1))
)

(synth-fun f ((l1 List) (x Int)) List
    ((Start List) (Ix Int))
    ((Start List ((Cons Ix Start) l1 Nil))
    (Ix Int (x 0)))
)

(declare-const a Int)
(declare-const b Int)
(declare-const c Int)

(constraint
    (set-equals (Cons a (Cons b Nil)) (f (Cons a Nil) b))
)
(constraint
    (set-equals (Cons c (Cons a (Cons b Nil))) (f (Cons c (Cons a Nil)) b))
)
(check-synth)
