(set-option :print-success true)
(set-logic ALL)
(set-option :quant-ind true)
(set-option :produce-models true)
(set-option :incremental true)
(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))
(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))

(declare-datatype tree_memo
  (
    (MNode (MNode_0 Int) (MNode_1 Int) (MNode_2 tree_memo) (MNode_3 tree_memo))
    (MLeaf (MLeaf_0 Int))
  )
)

(declare-datatype tree
  (
    (Node (Node_0 Int) (Node_1 tree) (Node_2 tree))
    (Leaf (Leaf_0 Int))
  )
)

(declare-fun is_memo (tree_memo) Bool)
(declare-fun memo (tree_memo) Int)
(assert (forall ((x Int)) (= (is_memo (MLeaf x)) true)))
(assert (forall ((n Int) (a Int) (l tree_memo) (r tree_memo))
    (= (is_memo (MNode n a l r))
     (and (= n (+ (+ 1 (memo l)) (memo r))) (and (is_memo l) (is_memo r))))))
(assert (forall ((x Int)) (= (memo (MLeaf x)) 1)))
(assert (forall ((n Int) (a Int) (l tree_memo) (r tree_memo))
    (= (memo (MNode n a l r)) n)))

(declare-fun repr (tree_memo) tree)
(assert (forall ((a Int)) (= (repr (MLeaf a)) (Leaf a))))
(assert (forall ((n Int) (a Int) (l tree_memo) (r tree_memo))
  (= (repr (MNode n a l r)) (Node a (repr l) (repr r)))))

(declare-fun spec (tree) Int)
(assert (forall ((a Int)) (= (spec (Leaf a)) 1)))
(assert (forall ((a Int) (l tree) (r tree))
  (= (spec (Node a l r)) (+ (+ 1 (spec l)) (spec r)))))


(define-fun lemma_0 ((i Int) (i1892 Int) (i1893 Int)) Bool
  (= i (+ (+ i1892 i1893) 1)))


(declare-fun f (tree_memo) Bool)
(assert
  (forall ((t tree_memo))
    (=
      (f t)
      (or
        (not (is_memo t))
        (= (memo t) (spec (repr t)))
      )
    )
  )
)


(assert
  (not
     (forall ((t tree_memo))
       (f t)
     )
  )
 )

(check-sat)
