(set-logic DTLIA)
(define-fun max ((x Int) (y Int)) Int (ite (> x y) x y))

(synth-fun join1_1 ((x63 Int) (x64 (Tuple Int Int)) (x65 (Tuple Int Int))) Int
 ((Ix Int) (Ix2 Int) (Ivar Int) (Ic Int))
 (
  (Ix Int
   (Ic x63 ((_ tupSel 0) x64) ((_ tupSel 1) x64) ((_ tupSel 0) x65) ((_ tupSel 1) x65)
    (- Ix2) (+ Ix2 Ix2) (max Ix2 Ix2)))
  (Ix2 Int
   (Ic x63 ((_ tupSel 0) x64) ((_ tupSel 1) x64) ((_ tupSel 0) x65) ((_ tupSel 1) x65)
    (- Ivar) (+ Ivar Ivar) (max Ivar Ivar)))
  (Ivar Int (Ic x63 ((_ tupSel 0) x64) ((_ tupSel 1) x64) ((_ tupSel 0) x65) ((_ tupSel 1) x65)))
  (Ic Int (0 1))))

(synth-fun join1_2 ((x63 Int) (x64 (Tuple Int Int)) (x65 (Tuple Int Int))) Int
 ((Ix Int) (Ix2 Int) (Ivar Int) (Ic Int))
 (
  (Ix Int
   (Ic x63 ((_ tupSel 0) x64) ((_ tupSel 1) x64) ((_ tupSel 0) x65) ((_ tupSel 1) x65)
    (- Ix2) (+ Ix2 Ix2) (max Ix2 Ix2)))
  (Ix2 Int
   (Ic x63 ((_ tupSel 0) x64) ((_ tupSel 1) x64) ((_ tupSel 0) x65) ((_ tupSel 1) x65)
    (- Ivar) (+ Ivar Ivar) (max Ivar Ivar)))
  (Ivar Int (Ic x63 ((_ tupSel 0) x64) ((_ tupSel 1) x64) ((_ tupSel 0) x65) ((_ tupSel 1) x65)))
  (Ic Int (0 1))))


(declare-var a Int)
(declare-var ex Int)
(declare-var i_59 Int)
(declare-var i_61 Int)

(constraint
 (or (not (and (>= i_61 0) (>= i_61 i_59)))
  (= (+ i_59 a) (join1_1 a (mkTuple i_59 i_61) (mkTuple 0 0)))))

(constraint
 (or (not (and (>= i_61 0) (>= i_61 i_59)))
  (= (max (+ i_59 a) i_61) (join1_2 a (mkTuple i_59 i_61) (mkTuple 0 0)))))

(constraint
 (or (not (and (>= i_61 0) (>= i_61 i_59)))
  (=  (+ (+ i_59 a) ex)
   (join1_1 a (mkTuple i_59 i_61) (mkTuple ex (max ex 0))))))

(constraint
 (or (not (and (>= i_61 0) (>= i_61 i_59)))
  (= (max (+ (+ i_59 a) ex) (max (+ i_59 a) i_61))
   (join1_2 a (mkTuple i_59 i_61) (mkTuple ex (max ex 0))))))



(check-synth)
