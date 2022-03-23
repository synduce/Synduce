open Base
open Lang
open Rewriter
open Expression
open Fmt

let assert_fac_expand e1 e2 =
  let fe2 = factorize e2 in
  let ee1 = expand e1 in
  let b1 = equal e1 fe2 in
  let b2 = equal ee1 e2 in
  if b1 && b2 then () else raise (Assert_failure ("assert_fac_expand", 0, 0))
;;

let assert_eequals e1 e2 =
  if eequals e1 e2 then () else raise (Assert_failure ("assert_eequals", 0, 0))
;;

let assert_is_subexpr ?(lemma = None) e1 e2 =
  match match_as_subexpr ~lemma (Position 0) e1 ~of_:e2 with
  | Some _ -> ()
  | None -> raise (Assert_failure ("assert_is_subexpr", 0, 0))
;;

let test_0 () =
  let a, b, c, u, v, w, x, y, z =
    let open Op in
    var 1, var 2, var 3, var 4, var 5, var 6, var 7, var 8, var 9
  in
  (* Addition, substraction *)
  assert_eequals Op.(a + int 0) a;
  assert_eequals Op.(a + (b + int 0)) Op.(a + b);
  assert_eequals Op.(x + y) Op.(y + x);
  assert_eequals Op.(u - int 0) u;
  assert_eequals Op.(a + b + c) Op.(c + b + a);
  (* Multiplication *)
  assert_eequals Op.(x * y * z) Op.(z * y * x);
  assert_eequals Op.(a * (b + c)) Op.((a * b) + (a * c));
  assert_eequals Op.((x + y) * (b + c)) Op.((x * b) + (y * b) + (x * c) + (y * c));
  (* Min and Max *)
  assert_eequals Op.(max a b) Op.(max b a);
  assert_eequals Op.(min x y) Op.(min y x);
  assert_eequals Op.(max v (max w z)) Op.(max (max v z) w);
  assert_eequals Op.(max (max x (int 0) + y) (int 0)) Op.(max (max (x + y) y) (int 0));
  assert_eequals Op.(~?(a > b) a b) Op.(max a b);
  assert_eequals Op.(~?(a < b) a b) Op.(min a b);
  assert_eequals Op.(~?(a <= b) a b) Op.(min a b);
  assert_eequals Op.(~?(a >= b) a b) Op.(max a b);
  assert_eequals Op.(~?(b < a) a b) Op.(max a b);
  assert_eequals Op.(~?(b > a) a b) Op.(min a b);
  (* If-then-else *)
  assert_eequals
    Op.(~?(x > int 1) (x + ~?(y > int 0) y (int 0)) (~?(y > int 0) y (int 0)))
    Op.(~?(x > int 1) x (int 0) + ~?(y > int 0) y (int 0));
  assert_eequals Op.(~?a x y > x) Op.((not a) && y > x);
  assert_eequals Op.(~?a x y > y) Op.(a && x > y);
  (* And, or *)
  assert_eequals Op.(x && ETrue) x;
  assert_eequals Op.(x && y && EFalse) EFalse;
  assert_eequals Op.(x || ETrue || y) ETrue;
  assert_eequals Op.(x || EFalse) x;
  assert_eequals Op.(x || EFalse || y) Op.(x || y);
  (* Implies *)
  assert_eequals Op.(x && x => y) Op.(x && y);
  assert_eequals Op.((x && y) && x => y) Op.(x && y);
  assert_eequals Op.(x && (not x) => y) x;
  assert_eequals Op.(x && z && (not x) => y) Op.(x && z)
;;

let test_1 () =
  let a, b, c, u, v, w, x, y, z =
    let open Op in
    var 1, var 2, var 3, var 4, var 5, var 6, var 7, var 8, var 9
  in
  assert_is_subexpr a Op.(a + b);
  assert_is_subexpr a Op.(a + b + c);
  assert_is_subexpr Op.(a + int 0) Op.(a + b);
  assert_is_subexpr Op.(a + b + int 0) Op.(a + b + c);
  assert_is_subexpr Op.(a + int 1) Op.(a + int 1 + int 0 + b + v);
  assert_is_subexpr Op.(u * (v + int 1)) Op.((u * v) + u + (u * v * w));
  assert_is_subexpr
    Op.(max (max (x + int 0) (int 0) + y) (int 0))
    Op.(max (x + y + z) (max (x + y) (max y (int 0))))
;;

let test_2 () =
  let a, b, c, u, v, w, x, y, z =
    let open Op in
    var 1, var 2, var 3, var 4, var 5, var 6, var 7, var 8, var 9
  in
  assert_fac_expand Op.(u * (v + w)) Op.((u * v) + (u * w));
  assert_fac_expand Op.(x + max y z) Op.(max (x + y) (x + z));
  assert_fac_expand Op.(x + ~?a b c) Op.(~?a (x + b) (x + c));
  assert_fac_expand
    Op.(normalize (x + max (y + max z (int 0)) (int 0)))
    Op.(normalize (max x (max (x + y) (x + y + z))))
;;

set_style_renderer stdout `Ansi_tty;
test_0 ();
test_1 ();
test_2 ()
