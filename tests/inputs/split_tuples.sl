(set-logic DTLIA)
(define-fun max ((x Int) (y Int)) Int (ite (> x y) x y))

(synth-fun odot_1 ((x52 (Tuple Int Int Int)) (x53 (Tuple Int Int Int))) Int
 ((Ix Int) (Ic Int))
 ((Ix Int (Ic ((_ tupSel 0) x52) ((_ tupSel 1) x52) ((_ tupSel 2) x52) ((_ tupSel 0) x53) ((_ tupSel 1) x53) ((_ tupSel 2) x53) (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int (0 1))))

(synth-fun odot_2 ((x52 (Tuple Int Int Int)) (x53 (Tuple Int Int Int))) Int
 ((Ix Int) (Ic Int))
 ((Ix Int (Ic ((_ tupSel 0) x52) ((_ tupSel 1) x52) ((_ tupSel 2) x52) ((_ tupSel 0) x53) ((_ tupSel 1) x53) ((_ tupSel 2) x53) (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int (0 1))))

(synth-fun odot_3 ((x52 (Tuple Int Int Int)) (x53 (Tuple Int Int Int))) Int
 ((Ix Int) (Ic Int))
 ((Ix Int (Ic ((_ tupSel 0) x52) ((_ tupSel 1) x52) ((_ tupSel 2) x52) ((_ tupSel 0) x53) ((_ tupSel 1) x53) ((_ tupSel 2) x53) (- Ix) (+ Ix Ix) (max Ix Ix)))
  (Ic Int (0 1))))

(declare-var ex44 Int)
(declare-var i_ Int)
(declare-var i_48 Int)
(declare-var i_50 Int)

(constraint
 (or (not (and (>= i_50 0) (and (>= i_48 0) (and (>= i_48 i_) (>= i_50 i_)))))
  (= i_ (odot_1 (mkTuple 0 0 0) (mkTuple i_ i_48 i_50)))))

(constraint
 (or (not (and (>= i_50 0) (and (>= i_48 0) (and (>= i_48 i_) (>= i_50 i_)))))
  (= i_48 (odot_2 (mkTuple 0 0 0) (mkTuple i_ i_48 i_50)))))

(constraint
 (or (not (and (>= i_50 0) (and (>= i_48 0) (and (>= i_48 i_) (>= i_50 i_)))))
  (= i_50 (odot_3 (mkTuple 0 0 0) (mkTuple i_ i_48 i_50)))))

(constraint
 (or (not (and (>= i_50 0) (and (>= i_48 0) (and (>= i_48 i_) (>= i_50 i_)))))
  (= (+ i_ ex44)
   (odot_1 (mkTuple ex44 (max ex44 0) (max ex44 0)) (mkTuple i_ i_48 i_50)))))

(constraint
 (or (not (and (>= i_50 0) (and (>= i_48 0) (and (>= i_48 i_) (>= i_50 i_)))))
  (= (max (+ i_48 ex44) 0)
   (odot_2 (mkTuple ex44 (max ex44 0) (max ex44 0)) (mkTuple i_ i_48 i_50)))))

(constraint
 (or (not (and (>= i_50 0) (and (>= i_48 0) (and (>= i_48 i_) (>= i_50 i_)))))
  (= (max i_50 (+ i_ ex44))
   (odot_3 (mkTuple ex44 (max ex44 0) (max ex44 0)) (mkTuple i_ i_48 i_50)))))

(check-synth)
