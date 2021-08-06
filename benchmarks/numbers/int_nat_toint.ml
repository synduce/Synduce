(* First representation of integers: positive or negative natural numbers. *)

type nat = Z | S of nat

type inat = P of nat | N of nat

let rec itoint = function N n -> -(nsum n + 1) | P n -> nsum n

and nsum = function Z -> 0 | S n -> 1 + nsum n

(* Second representation: counting from zero down or up *)

type eint = Zero | Sub1 of eint | Add1 of eint

let rec repr = function Zero -> P Z | Add1 n -> radd1 (repr n) | Sub1 n -> rsub1 (repr n)

and radd1 = function P n -> padd1 n | N n -> nadd1 n

and rsub1 = function P n -> psub1 n | N n -> nsub1 n

and padd1 = function Z -> P (S Z) | S n -> P (S (S n))

and nadd1 = function Z -> P Z | S n -> N n

and psub1 = function Z -> N Z | S n -> P n

and nsub1 = function Z -> N (S Z) | S n -> N (S (S n))

let rec etoint = function
  | Zero -> [%synt c0]
  | Sub1 n -> [%synt sub1] (etoint n)
  | Add1 n -> [%synt add1] (etoint n)
;;

assert (etoint = repr @ itoint)
