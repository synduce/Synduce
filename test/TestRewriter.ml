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
  if b1 && b2
  then ()
  else (
    pf stdout "%a@." (styled (`Fg `Red) string) "Assertion failure:";
    pf stdout "e1 = %a@." pp e1;
    pf stdout "e2 = %a@." pp e2;
    if not b1
    then pf stdout "e1 should be e2 factorized, but (factorize e2)  = %a@." pp fe2;
    if not b2 then pf stdout "e1 expanded should be e2, but (expand e1)  = %a@." pp ee1;
    raise (Assert_failure ("assert_fac_expand", 0, 0)))
;;

let assert_eequals e1 e2 =
  if eequals e1 e2
  then ()
  else (
    pf stdout "%a@." (styled (`Fg `Red) string) "Assertion failure:";
    pf stdout "e1 = %a@." pp e1;
    pf stdout "e2 = %a@." pp e2;
    pf stdout "e1 and e2 should be e-equal, but they are not.@.";
    pf stdout "After simplification and expansion:@.";
    pf stdout "e1 = %a@." pp (expand (factorize (simplify e1)));
    pf stdout "e2 = %a@." pp (expand (factorize (simplify e2)));
    raise (Assert_failure ("assert_equals", 0, 0)))
;;

let e_to_expand =
  let open Op in
  [ var 1 * (var 2 + var 3)
  ; var 1 * ((var 2 * int 5) + var 3)
  ; max (max (max (var 22) (int 0) + var 5) (int 0) + var 3) (int 0)
  ; max (max (var 5) (int 0) + var 3) (int 0)
  ; max (max (max (var 1 + var 2) (int 0) + var 3) (int 0) + var 4) (int 0)
  ; var 1 * (var 2 + var 3) * int 2
  ; ~?(var 4) (var 1 * (var 2 + var 3)) (var 2)
  ; var 1 && (var 1 || var 3)
  ]
;;

let e_to_factorize =
  let open Op in
  [ (var 1 * var 2) + (var 1 * var 3)
  ; (var 1 * var 2) + (var 1 * var 3) + (var 1 * var 3 * var 4)
  ; max (max (var 1 + var 2) (var 1 + var 3)) (var 1 + var 3 + var 4)
  ; max (var 1 + var 1 + (var 1 * var 2)) ((var 1 * int 2) + (var 1 * var 2))
  ; var 1 + var 1 + var 1
  ]
;;

let e_pairs_to_elim =
  let open Op in
  [ ( max (max (max (var 22) (int 0) + var 5) (int 0) + var 3) (int 0)
    , max (max (var 5) (int 0) + var 3) (int 0) )
  ; ( var 5 * max (max (max (var 22) (int 0) + var 5) (int 0) + var 3) (int 0)
    , max (max (var 5) (int 0) + (int 0 + var 3)) (int 0) )
  ]
;;

let test_0 () =
  let a, b, c, _u, v, w, x, y, z =
    let open Op in
    var 1, var 2, var 3, var 4, var 5, var 6, var 7, var 8, var 9
  in
  assert_eequals Op.(a + int 0) a;
  assert_eequals Op.(a + (b + int 0)) Op.(a + b);
  assert_eequals Op.(max a b) Op.(max b a);
  assert_eequals Op.(min x y) Op.(min y x);
  assert_eequals Op.(x + y) Op.(y + x);
  (* assert_eequals Op.(u - int 0) u; *)
  assert_eequals Op.(a + b + c) Op.(c + b + a);
  assert_eequals Op.(max v (max w z)) Op.(max (max v z) w);
  assert_eequals Op.(max (max x (int 0) + y) (int 0)) Op.(max (max (x + y) y) (int 0));
  assert_eequals
    Op.(~?(x > int 1) (x + ~?(y > int 0) y (int 0)) (~?(y > int 0) y (int 0)))
    Op.(~?(x > int 1) x (int 0) + ~?(y > int 0) y (int 0))
;;

let test_1 () =
  set_style_renderer stdout `Ansi_tty;
  Caml.Format.set_margin 100;
  pf stdout "Expand@.";
  List.iteri e_to_expand ~f:(fun i e ->
      pf
        stdout
        "@[%a: %a --->@;<1 4>%a@]@."
        (styled (`Bg `Magenta) int)
        i
        pp
        e
        pp
        (expand e));
  pf stdout "Factorize@.";
  List.iteri e_to_factorize ~f:(fun i e ->
      pf
        stdout
        "@[%a: %a --->@;<1 4>%a@]@."
        (styled (`Bg `Magenta) int)
        i
        pp
        e
        pp
        (factorize e));
  pf stdout "Match subexpression@.";
  List.iteri e_pairs_to_elim ~f:(fun i (e, esub) ->
      pf stdout "@[%a: %a --->@;<1 4>%a@]@." (styled (`Bg `Magenta) int) i pp e pp esub;
      match match_as_subexpr (Position i) esub ~of_:e with
      | Some t -> pf stdout "%i -> %a@." i pp t
      | None -> pf stdout "No.@.")
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

test_0 ();
test_1 ();
test_2 ()
