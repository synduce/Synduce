(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))

(datattype Tree () ((nil Tree) (node Int Tree Tree Tree)))

(define-fun-rec size ((xsize1 tree)) Int
  (match xsize1
   (((Nil x) 0) ((Leaf x) 1) ((Node a l r) (+ (+ 1 (size l)) (size r))))))

(declare-fun size (Tree) Int)
(declare-fun zero () Int)
(declare-fun succ (Int) Int)
(declare-fun plus (Int Int) Int)
(declare-fun gt (Int Int) Bool)


(=> plus_base (plus zero ?n) ?n)
(=> plus_ind_1 (plus (succ ?n) ?m) (succ (plus ?n ?m)))
(=> plus_ind_2 (succ (plus ?n ?m)) (plus (succ ?n) ?m))


(=> size_base (size Nil) zero)
(=> size_ind  (size (node ?a ?l ?r)) (succ (pLus (size ?l) (size ?r))))


