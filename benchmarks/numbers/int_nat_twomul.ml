(* First representation of integers: positive or negative natural numbers. *)

type nat = Z | S of nat

type inat = P of nat | N of nat

type two_inats = TwoInats of inat * inat

(* Reference function: sum two inats. *)
let rec two_imul = function TwoInats (n1, n2) -> ival n1 * ival n2

and ival = function N n -> -(nval n + 1) | P n -> nval n

and nval = function Z -> 0 | S n -> 1 + nval n

(* Second representation: counting from zero down or up *)

type eint = Zero | Sub1 of eint | Add1 of eint

type two_ints = TwoInts of eint * eint

let rec repr = function TwoInts (i1, i2) -> TwoInats (irepr i1, irepr i2)

and irepr = function Zero -> P Z | Add1 n -> radd1 (irepr n) | Sub1 n -> rsub1 (irepr n)

and radd1 = function P n -> padd1 n | N n -> nadd1 n

and rsub1 = function P n -> psub1 n | N n -> nsub1 n

and padd1 = function Z -> P (S Z) | S n -> P (S (S n))

and nadd1 = function Z -> P Z | S n -> N n

and psub1 = function Z -> N Z | S n -> P n

and nsub1 = function Z -> N (S Z) | S n -> N (S (S n))

(* Target: some recursive function that compute the sum of two eints. *)

let rec two_emul = function TwoInts (i1, i2) -> [%synt mul] (to_int i1) (to_int i2)

and to_int = function
  | Zero -> [%synt c0]
  | Sub1 n -> [%synt sub1] (to_int n)
  | Add1 n -> [%synt add1] (to_int n)
;;

assert (two_emul = repr @ two_imul)
